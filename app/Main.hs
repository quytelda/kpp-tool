{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Control.Monad
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
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

inputFile :: Parser FilePath
inputFile = argument str (metavar "FILE")

-- | Runtime Commands
data Command = CmdShow
             | CmdVerify
             | CmdParams  [String]
             | CmdExtract String (Maybe FilePath)
             | CmdRename  String (Maybe FilePath)
             deriving (Show)

-- | Create a command subparser option that can be passed to 'subparser'.
mkCommand :: String -> Parser Command -> String -> Mod CommandFields Command
mkCommand name parser = command name . info (parser <**> helper) . progDesc

subcommand :: Parser Command
subcommand = subparser $ mconcat
  [ mkCommand "show"    cmdShow    "Display a summary of preset settings"
  , mkCommand "verify"  cmdVerify  "Verify embedded resource checksums"
  , mkCommand "rename"  cmdRename  "Change the name of a preset"
  , mkCommand "extract" cmdExtract "Extract an embedded resource file"
  ]
  where
    cmdShow    = pure CmdShow <|> cmdParams
    cmdVerify  = pure CmdVerify
    cmdParams  = CmdParams  <$> many param
    cmdRename  = CmdRename  <$> argument str (metavar "NAME")
                            <*> optional output
    cmdExtract = CmdExtract <$> argument str (metavar "NAME")
                            <*> optional output

-- | 'Options' represents a selection of runtime configuration
-- options and arguments.
data Options = Options
  { optCommand :: Command
  , optInput   :: FilePath
  } deriving (Show)

options :: Parser Options
options = Options
  <$> subcommand
  <*> inputFile
  <**> helper
  <**> versioner

parserInfo :: ParserInfo Options
parserInfo = info options
  ( fullDesc
  <> header "kpp-tool - a CLI tool for inspecting and editing KPP files"
  )

-- | Handler for CmdShow Commands
runShow :: Preset -> IO ()
runShow = TLIO.putStrLn . describe

-- | Handler for CmdVerify Commands
runVerify :: Preset -> IO ()
runVerify = void . Map.traverseWithKey printStatus . verifyResourceChecksums
  where
    printStatus name match = TIO.putStrLn $
      name <> ": " <> if match
                      then "OK"
                      else "Checksum Mismatch"

-- | Handler for CmdRename Commands
runRename :: String -> FilePath -> Preset -> IO ()
runRename newName path preset = do
  bytes <- encode $ setPresetName preset newName'
  BL.writeFile path bytes
  putStrLn $ "Renamed " <> show oldName <> " to " <> show newName <> "."
  where
    oldName  = presetName preset
    newName' = T.pack newName
    encode = either fail return . encodeKPP

-- | Handler for CmdParams Commands
runParams :: [String] -> Preset -> IO ()
runParams params preset = forM_ params $ \key ->
  case getParam (T.pack key) preset of
    Nothing  -> fail $ "unrecognized parameter: " <> key
    Just val -> TLIO.putStrLn $ describe val

-- | Handler for CmdExtract Commands
runExtract :: String -> Maybe FilePath -> Preset -> IO ()
runExtract name mpath preset =
  case getResource (T.pack name) preset of
    Nothing           -> fail $ "no such resource: " <> name
    Just Resource{..} -> let path = fromMaybe (T.unpack resourceFile) mpath
                         in BS.writeFile path resourceData

main :: IO ()
main = do
  opts@Options{..} <- execParser parserInfo

  -- argument debugging info
  putStrLn $ show opts
  putStrLn $ replicate 80 '-'

  -- load input file
  contents <- BS.readFile optInput
  preset   <- either fail return $ decodeKPP contents

  -- Run the selected operation.
  case optCommand of
    CmdShow                 -> runShow preset
    CmdVerify               -> runVerify preset
    CmdParams params        -> runParams params preset
    CmdExtract name mpath   -> runExtract name mpath preset
    CmdRename oldName mpath -> let path = fromMaybe optInput mpath
                               in runRename oldName path preset

  return ()
