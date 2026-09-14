{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Mangrove

import           Kpp.App

main :: IO ()
main = parseArguments programInfo parseSettings run
  where
    programInfo = ProgramInfo
      { programName = "kpp-tool"
      , programDesc = "A utility for interacting with Krita brush presets from the command line"
      , programVersion = kppToolVersion
      }
