{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Common
  ( pngIHDRChunkSize
  , testDir
  , withTestDir
  , path_basicEllipse
  , path_basicShapeGrainy
  , path_levels
  , path_scribble
  , path_1px
  , levelsFilterConfig
  ) where

import           Control.Exception
import           Data.Int          (Int64)
import qualified Data.Map.Strict   as Map
import           System.Directory

import           Kpp.Preset

pngIHDRChunkSize :: Int64
pngIHDRChunkSize = 17

-- | This is the directory where temporary test files will be created
-- during unit tests. It is automatically created when tests are run,
-- then deleted after the tests complete.
testDir :: FilePath
testDir = "test.tmp"

withTestDir :: IO () -> IO ()
withTestDir = bracket_
  (createDirectory testDir)
  (removeDirectoryRecursive testDir)

----------------------------
-- Sample Files for Tests --
----------------------------

path_basicEllipse :: FilePath
path_basicEllipse = "kpp/basic-ellipse.kpp"

path_basicShapeGrainy :: FilePath
path_basicShapeGrainy = "kpp/basic-shape-grainy.kpp"

path_levels :: FilePath
path_levels = "kpp/levels.kpp"

path_scribble :: FilePath
path_scribble = "png/scribble.png"

path_1px :: FilePath
path_1px = "png/1px.png"

-------------------
-- Sample Values --
-------------------

-- Filter settings from levels.kpp
levelsFilterConfig :: FilterConfig
levelsFilterConfig = FilterConfig "2" $ Map.fromList
  [ ("blackvalue",         Internal "7")
  , ("channel_0",          Unknown "0;1;1;0;1")
  , ("channel_1",          Unknown "0;1;1;0;1")
  , ("channel_2",          Unknown "0;1;1;0;1")
  , ("channel_3",          Unknown "0;1;1;0;1")
  , ("channel_4",          Unknown "0;1;1;0;1")
  , ("channel_5",          Unknown "0;1;1;0;1")
  , ("channel_6",          Unknown "0;1;1;0;1")
  , ("channel_7",          Unknown "0;1;1;0;1")
  , ("gammavalue",         Internal "0.681020988537474")
  , ("histogram_mode",     Unknown "linear")
  , ("lightness",          Unknown "0.0277777777777778;1;0.681020988537474;\
                                   \0.0925925925925926;0.580246913580247")
  , ("mode",               Unknown "lightness")
  , ("number_of_channels", Unknown "8")
  , ("outblackvalue",      Internal "24")
  , ("outwhitevalue",      Internal "148")
  , ("whitevalue",         Internal "255")
  ]
