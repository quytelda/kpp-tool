module Main (main) where

import           Options.Applicative

import           Preset              ()

parser :: ParserInfo a
parser = info (empty <**> helper)
         (  fullDesc
         <> header "kpp-tool - a CLI tool for inspecting and editing KPP files"
         )

main :: IO ()
main = undefined
