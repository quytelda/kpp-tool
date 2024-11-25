{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : PNG
Copyright   : (c) Quytelda Kahja, 2024
License     : BSD-3-Clause

This module contains functions and data structures for parsing and
rendering PNG files.
-}
module KPP.PNG where

import           Codec.Compression.Zlib
import           Control.Applicative
import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString        as BS
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.ByteString.Lazy   as BL
import           Data.Digest.CRC32

-- | Similar to `runGetOrFail` but uses MonadFail and doesn't include
-- information about how much information was consumed.
runGetOrFail' :: MonadFail f => Get a -> ByteString -> f a
runGetOrFail' g input = case runGetOrFail g input of
  Right (_, _, result) -> pure result
  Left  (_, _, err)    -> fail err

getNull :: Get ()
getNull = do
  b <- getWord8
  guard $ b == 0

putNull :: Put
putNull = putWord8 0

-- | Parse a known string.
expect :: ByteString -> Get ()
expect expected = do
  actual <- getLazyByteString $ BL.length expected
  unless (actual == expected) $
    fail $ "expected " <> show expected

pngMagicString :: ByteString
pngMagicString = "\x89\x50\x4E\x47\x0D\x0A\x1A\x0A"

-- | Check whether a `BS.ByteString` represents a PNG image.
--
-- This function doesn't attempt to fully parse or validate the input.
-- It simply checks whether the input begins with the standard PNG
-- magic string.
isPngData :: BS.ByteString -> Bool
isPngData bs = BS.toStrict pngMagicString `BS.isPrefixOf` bs

getMagicString :: Get ()
getMagicString = expect pngMagicString

putMagicString :: Put
putMagicString = putLazyByteString pngMagicString

-- | Tags for PNG chunk content which are relevant for parsing KPP
-- files.
data Chunk = VersionChunk ByteString
           | SettingChunk ByteString
           | RegularChunk ByteString
           deriving (Show)

getTextChunk :: ByteString -> Get ByteString
getTextChunk key = do
  expect "tEXt"
  expect key *> getNull
  getRemainingLazyByteString

putTextChunk :: ByteString -> ByteString -> Put
putTextChunk key value = do
  putLazyByteString "tEXt"
  putLazyByteString key *> putNull
  putLazyByteString value

getZtxtChunk :: ByteString -> Get ByteString
getZtxtChunk keyword = do
  expect "zTXt"
  expect keyword *> getNull
  void getWord8 -- compression type is always 0
  decompress <$> getRemainingLazyByteString

putZtxtChunk :: ByteString -> ByteString -> Put
putZtxtChunk key value = do
  putLazyByteString "zTXt"
  putLazyByteString key *> putNull
  putWord8 0 -- compression type is always 0
  putLazyByteString $ compress value

getItxtChunk :: ByteString -> Get ByteString
getItxtChunk keyword = do
  expect "iTXt"
  expect keyword *> getNull
  compressed <- get :: Get Bool
  void getWord8 -- compression type is always 0
  void getLazyByteStringNul -- ignore language tag
  void getLazyByteStringNul -- ignore translated keyword
  content <- getRemainingLazyByteString

  return $ if compressed
           then decompress content
           else content

putItxtChunk :: Bool -> ByteString -> ByteString -> Put
putItxtChunk compressed keyword value = do
  putLazyByteString "iTXt"
  putLazyByteString keyword *> putNull
  put compressed -- compression
  putWord8 0 -- compression type is always 0
  putLazyByteString "en_US.UTF-8" *> putNull -- language tag
  putLazyByteString keyword       *> putNull -- translated keyword
  putLazyByteString $
    if compressed
    then compress value
    else value

getIhdrDimensions :: Get (Word32, Word32)
getIhdrDimensions = do
  expect "IHDR"
  width  <- getWord32be
  height <- getWord32be
  return (width, height)

getKeywordChunk :: ByteString -> Get ByteString
getKeywordChunk key = getTextChunk key <|>
                      getZtxtChunk key <|>
                      getItxtChunk key

getChunk :: Get Chunk
getChunk = do
  chunkLength <- getWord32be
  chunkData   <- getLazyByteString $ fromIntegral (4 + chunkLength)
  chunkCsum   <- getWord32be

  if chunkCsum == crc32 chunkData
    then runGetOrFail' getChunkData chunkData
    else fail "checksum mismatch"
  where
    getChunkData = VersionChunk <$> getKeywordChunk "version" <|>
                   SettingChunk <$> getKeywordChunk "preset"  <|>
                   RegularChunk <$> getRemainingLazyByteString

putChunk :: Chunk -> Put
putChunk chunk = do
  putWord32be chunkLength
  putLazyByteString chunkData
  putWord32be chunkCsum
  where
    chunkData = runPut $ case chunk of
      VersionChunk version -> putTextChunk "version" version
      SettingChunk xml     -> putZtxtChunk "preset"  xml
      RegularChunk bytes   -> putLazyByteString bytes
    chunkLength = fromIntegral $ BL.length chunkData - 4
    chunkCsum   = crc32 chunkData

instance Binary Chunk where
  get = getChunk
  put = putChunk

-- | Extract just a preset's XML settings data.
parseSettingsXml :: MonadFail m => BL.ByteString -> m BL.ByteString
parseSettingsXml = runGetOrFail' $ do
  getMagicString
  chunks <- some getChunk
  case [p | SettingChunk p <- chunks] of
    [s] -> return s
    [] -> fail "missing settings chunk"
    _  -> fail "too many settings chunks"
