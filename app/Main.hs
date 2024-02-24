{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import           Control.Monad
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as T
import qualified Data.Text.Lazy.IO    as TLIO
import           Data.Version         (showVersion)
import           Options.Applicative

import           Describe
import           Preset

-- | A hidden version option (similar to 'helper') that displays the
-- program version.
versioner :: Parser (a -> a)
versioner = simpleVersioner $ showVersion version

output :: Parser FilePath
output = strOption
  (  long "output"
  <> short 'o'
  <> help "Write output to FILE instead of stdout"
  )

param :: Parser String
param = strOption
  (  long "param"
  <> short 'p'
  <> help "Display the value of a preset parameter"
  )

-- | Runtime Operations
data Operation = OpShow -- ^ Display a summary of information about a
                        -- preset.
               | OpParams  [String]
               | OpRename String (Maybe FilePath)
               deriving (Show)

showOption :: Parser Operation
showOption = flag' OpShow
  (  long "show"
  <> short 's'
  <> help "Display a summary of preset settings"
  )

paramsOption :: Parser Operation
paramsOption = OpParams <$> many param

renameOption :: Parser Operation
renameOption = OpRename
  <$> strOption (  long "rename"
                <> short 'r'
                <> help "Change the name of a preset"
                )
  <*> optional output

operation :: Parser Operation
operation = showOption <|> renameOption <|> paramsOption

inputFile :: Parser FilePath
inputFile = argument str (metavar "FILE")

-- | 'Options' represents a selection of runtime configuration
-- options and arguments.
data Options = Options
  { optOperation :: Operation
  , optInput     :: FilePath
  } deriving (Show)

options :: Parser Options
options = Options
  <$> operation
  <*> inputFile
  <**> helper
  <**> versioner

parser :: ParserInfo Options
parser = info options
  ( fullDesc
  <> header "kpp-tool - a CLI tool for inspecting and editing KPP files"
  )

-- | Handler for OpShow Operations
runShow :: Preset -> IO ()
runShow = TLIO.putStrLn . describe

-- | Handler for OpRename Operations
runRename :: String -> FilePath -> Preset -> IO ()
runRename newName path preset = do
  bytes <- encode $ setPresetName preset newName'
  BL.writeFile path bytes
  putStrLn $ "Renamed " <> show oldName <> " to " <> show newName <> "."
  where
    oldName  = presetName preset
    newName' = T.pack newName
    encode = either fail return . encodeKPP

-- | Handler for OpParams Operations
runParams :: [String] -> Preset -> IO ()
runParams params preset = forM_ params $ \key ->
  case getParam (T.pack key) preset of
    Nothing  -> fail $ "unrecognized parameter: " <> key
    Just val -> TLIO.putStrLn $ describe val

main :: IO ()
main = do
  opts@Options{..} <- execParser parser

  -- argument debugging info
  putStrLn $ show opts
  putStrLn $ replicate 80 '-'

  -- load input file
  contents <- BS.readFile optInput
  preset   <- either fail return $ decodeKPP contents

  case optOperation of
    OpShow                 -> runShow preset
    OpRename oldName mpath -> let path = fromMaybe optInput mpath
                              in runRename oldName path preset
    OpParams params        -> runParams params preset

  return ()
