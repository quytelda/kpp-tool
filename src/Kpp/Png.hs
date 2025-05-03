{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : Kpp.Png
Copyright   : (c) Quytelda Kahja, 2024
License     : BSD-3-Clause

This module contains functions and data structures for parsing and
rendering PNG files.
-}
module Kpp.Png where

import           Codec.Compression.Zlib
import           Conduit
import           Control.Applicative
import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as BL
import qualified Data.Conduit.Combinators          as C
import           Data.Conduit.Serialization.Binary
import           Data.Digest.CRC32
import           Text.XML

import           Kpp.Common

--------------------------------------------------------------------------------
-- Binary Parsers and Renderers

getNull :: Get ()
getNull = label "getNull" $ do
  w <- getWord8
  unless (w == 0) $
    fail $ "expected 0, got " <> show w

putNull :: Put
putNull = putWord8 0

-- | Parse a known string.
expect :: ByteString -> Get ()
expect expected = do
  actual <- getByteString (BS.length expected)
  unless (actual == expected) $
    fail $ "expected " <> show expected <> ", got " <> show actual

-- | Unwrap a PNG chunk. The returned "inner chunk" consists of the
-- chunk type and chunk data.
--
-- Fails if the checksum verification is unsuccessful.
getChunk :: Get ByteString
getChunk = do
  chunkLength <- getWord32be
  chunkData   <- getByteString $ fromIntegral (4 + chunkLength)
  chunkCsum   <- getWord32be

  if chunkCsum == crc32 chunkData
    then pure chunkData
    else fail "checksum mismatch"

putChunk :: ByteString -> Put
putChunk chunkData = do
  putWord32be   chunkLength
  putByteString chunkData
  putWord32be   chunkCsum
  where
    chunkLength = fromIntegral $ BS.length chunkData - 4
    chunkCsum   = crc32 chunkData

-- | Parse a tEXt chunk with a matching key and return its content.
getTextChunk :: ByteString -> Get BL.ByteString
getTextChunk key = do
  expect "tEXt"
  expect key *> getNull
  getRemainingLazyByteString

-- | Build a tEXt chunk from a given key and value.
putTextChunk :: ByteString -> BL.ByteString -> Put
putTextChunk key value = do
  putByteString "tEXt"
  putByteString key *> putNull
  putLazyByteString value

-- | Parse a zTXt chunk with a matching key and return its decompressed content.
getZtxtChunk :: ByteString -> Get BL.ByteString
getZtxtChunk keyword = do
  expect "zTXt"
  expect keyword *> getNull
  void getWord8 -- compression type is always 0
  decompress <$> getRemainingLazyByteString

-- | Build a zTXt chunk from a given key and value.
putZtxtChunk :: ByteString -> BL.ByteString -> Put
putZtxtChunk key value = do
  putByteString "zTXt"
  putByteString key *> putNull
  putWord8 0 -- compression type is always 0
  putLazyByteString $ compress value

-- | Parse a iTXt chunk with a matching key and return its decompressed content.
--
-- The language tag and translated keyword fields are ignored.
getItxtChunk :: ByteString -> Get BL.ByteString
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

-- | Build a (possibly compressed) iTXt chunk from a given key and value.
putItxtChunk :: Bool -> ByteString -> BL.ByteString -> Put
putItxtChunk compressed keyword value = do
  putByteString "iTXt"
  putByteString keyword *> putNull
  put compressed -- compression
  putWord8 0 -- compression type is always 0
  putNull -- empty language tag
  putNull -- empty translated keyword
  putLazyByteString $
    if compressed
    then compress value
    else value

-- | Parse any tEXt, zTXt, or iTXt chunk with a matching keyword and
-- return its content.
getKeywordChunk :: ByteString -> Get BL.ByteString
getKeywordChunk key = getTextChunk key <|>
                      getZtxtChunk key <|>
                      getItxtChunk key

-- | Test whether this chunk is a textual chunk with a matching
-- keyword.
isKeywordChunk :: ByteString -> ByteString -> Bool
isKeywordChunk key bs = isTextualType && keywordMatches
  where
    (chunkType, chunkData) = BS.splitAt 4 bs
    isTextualType = chunkType == "tEXt" ||
                    chunkType == "zTXt" ||
                    chunkType == "iTXt"
    keywordMatches = (BS.append key "\0") `BS.isPrefixOf` chunkData

isKeywordChunk' :: ByteString -> ByteString -> Bool
isKeywordChunk' key bs = isTextualType && keywordMatches
  where
    (chunkType, chunkData) = BS.splitAt 4 (BS.drop 4 bs)
    isTextualType = chunkType == "tEXt" ||
                    chunkType == "zTXt" ||
                    chunkType == "iTXt"
    keywordMatches = (BS.append key "\0") `BS.isPrefixOf` chunkData

-- | Determine whether a chunk is a special chunk with KPP settings or
-- a regular PNG chunk that we can ignore.
isRegularChunk :: ByteString -> Bool
isRegularChunk bs = not $
  isKeywordChunk "version" bs || isKeywordChunk "preset" bs

-- | Extract the width and height of an image from an IHDR chunk.
getIhdrDimensions :: Get (Word32, Word32)
getIhdrDimensions = do
  expect "IHDR"
  width  <- getWord32be
  height <- getWord32be
  return (width, height)

--------------------------------------------------------------------------------
-- Conduits

-- | Divide a PNG data stream into a stream of unwrapped chunks.
pngToChunks :: MonadThrow m => ConduitT ByteString ByteString m ()
pngToChunks = do
  -- Every PNG starts with the same 8-byte magic string.
  magic <- takeCE 8 .| sinkLazy
  unless (magic == pngMagicString) $
    throwM $ ParseException "invalid PNG data"

  conduitGet getChunk

-- | Combine a stream of unwrapped chunks into a PNG data stream.
chunksToPng :: Monad m => ConduitT ByteString ByteString m ()
chunksToPng = do
  yield (BS.toStrict pngMagicString)
  C.map putChunk
    .| conduitPut

-- | Consume exactly one item from a stream, then fail if any input
-- remains unconsumed.
sinkExactly1 :: MonadThrow m => ConduitT a Void m a
sinkExactly1 = do
  x <- await <* endOfInput
  maybe (throwM emptyStreamError) pure x
  where
    endOfInput = do
      done <- C.null
      unless done $
        throwM extraInputError
    emptyStreamError = ParseException "empty stream"
    extraInputError  = ParseException "unconsumed input"

parseKeywordChunks :: MonadThrow m => ByteString -> ConduitT ByteString ByteString m ()
parseKeywordChunks key =
  C.filter (isKeywordChunk key)
  .| conduitGet (BS.toStrict <$> getKeywordChunk key)

parseVersionChunks :: MonadThrow m => ConduitT ByteString Void m ByteString
parseVersionChunks =
  parseKeywordChunks "version"
  .| sinkExactly1

parseSettingChunks :: MonadThrow m => ConduitT ByteString Void m Document
parseSettingChunks =
  parseKeywordChunks "preset"
  .| sinkDoc def

parseRegularChunks :: MonadThrow m => ConduitT ByteString Void m BL.ByteString
parseRegularChunks =
  C.filter isRegularChunk
  .| chunksToPng
  .| sinkLazy
