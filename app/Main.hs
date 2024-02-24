module Main (main) where

import           Data.Version        (showVersion)
import           Options.Applicative

import           Preset

-- | A hidden version option (similar to 'helper') that displays the
-- program version.
versioner :: Parser (a -> a)
versioner = simpleVersioner $ showVersion version

parser :: ParserInfo a
parser = info (empty <**> helper <**> versioner)
         (  fullDesc
         <> header "kpp-tool - a CLI tool for inspecting and editing KPP files"
         )

main :: IO ()
main = undefined
