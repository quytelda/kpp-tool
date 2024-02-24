module Main (main) where

import           Data.Version        (showVersion)
import           Options.Applicative

import           Preset

-- | A hidden version option (similar to 'helper') that displays the
-- program version.
versioner :: Parser (a -> a)
versioner = simpleVersioner $ showVersion version

-- | Runtime Operations
data Operation = OpShow -- ^ Display a summary of information about a
                        -- preset.
               deriving (Show)

showOption :: Parser Operation
showOption = flag' OpShow
  (  long "show"
  <> short 's'
  <> help "Display a summary of preset settings"
  )

operation :: Parser Operation
operation = showOption

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

main :: IO ()
main = undefined
