{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Common where

import           Control.Exception
import           Data.Int          (Int64)
import           System.Directory

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

path_scribble :: FilePath
path_scribble = "png/scribble.png"

path_1px :: FilePath
path_1px = "png/1px.png"
