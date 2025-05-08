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

data PngChunk = PngChunk
  { chunkType :: ByteString
  , chunkData :: BL.ByteString
  } deriving (Eq, Show)

chunkCRC :: PngChunk -> Word32
chunkCRC PngChunk{..} = crc32Update (crc32 chunkType) chunkData

-- | Unwrap a PNG chunk. The returned "inner chunk" consists of the
-- chunk type and chunk data.
--
-- Fails if the checksum verification is unsuccessful.
getChunk :: Get PngChunk
getChunk = do
  chunkLength <- getWord32be
  chunkType   <- getByteString 4
  chunkData   <- getLazyByteString $ fromIntegral chunkLength
  chunkCsum   <- getWord32be
  let chunk = PngChunk{..}

  if chunkCsum == chunkCRC chunk
    then pure chunk
    else fail "checksum mismatch"

putChunk :: PngChunk -> Put
putChunk chunk@PngChunk{..} = do
  putWord32be   chunkLength
  putByteString chunkType
  putLazyByteString chunkData
  putWord32be   chunkCsum
  where
    chunkLength = fromIntegral $ BL.length chunkData
    chunkCsum   = chunkCRC chunk

-- | Parse a tEXt chunk with a matching key and return its content.
getTextChunk :: ByteString -> Get BL.ByteString
getTextChunk key = do
  expect key *> getNull
  getRemainingLazyByteString

-- | Build a tEXt chunk from a given key and value.
putTextChunk :: ByteString -> BL.ByteString -> Put
putTextChunk key value = do
  putByteString key *> putNull
  putLazyByteString value

-- | Parse a zTXt chunk with a matching key and return its decompressed content.
getZtxtChunk :: ByteString -> Get BL.ByteString
getZtxtChunk keyword = do
  expect keyword *> getNull
  void getWord8 -- compression type is always 0
  decompress <$> getRemainingLazyByteString

-- | Build a zTXt chunk from a given key and value.
putZtxtChunk :: ByteString -> BL.ByteString -> Put
putZtxtChunk key value = do
  putByteString key *> putNull
  putWord8 0 -- compression type is always 0
  putLazyByteString $ compress value

-- | Parse a iTXt chunk with a matching key and return its decompressed content.
--
-- The language tag and translated keyword fields are ignored.
getItxtChunk :: ByteString -> Get BL.ByteString
getItxtChunk keyword = do
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
  putByteString keyword *> putNull
  put compressed -- compression
  putWord8 0 -- compression type is always 0
  putNull -- empty language tag
  putNull -- empty translated keyword
  putLazyByteString $
    if compressed
    then compress value
    else value

-- | Test whether this chunk is a textual chunk with a matching
-- keyword.
isKeywordChunk :: BL.ByteString -> PngChunk -> Bool
isKeywordChunk key PngChunk{..} = isTextualType && keywordMatches
  where
    isTextualType = chunkType == "tEXt" ||
                    chunkType == "zTXt" ||
                    chunkType == "iTXt"
    keywordMatches = (BL.append key "\0") `BL.isPrefixOf` chunkData

-- | Determine whether a chunk is a special chunk with KPP settings or
-- a regular PNG chunk that we can ignore.
isRegularChunk :: PngChunk -> Bool
isRegularChunk c = not $
  isKeywordChunk "version" c || isKeywordChunk "preset" c

-- | Extract the width and height of an image from an IHDR chunk.
getIhdrDimensions :: Get (Word32, Word32)
getIhdrDimensions = do
  width  <- getWord32be
  height <- getWord32be
  return (width, height)

--------------------------------------------------------------------------------
-- Conduits

-- | Divide a PNG data stream into a stream of unwrapped chunks.
pngToChunks :: MonadThrow m => ConduitT ByteString PngChunk m ()
pngToChunks = do
  -- Every PNG starts with the same 8-byte magic string.
  magic <- takeCE 8 .| sinkLazy
  unless (magic == pngMagicString) $
    throwM $ ParseException "invalid PNG data"

  conduitGet getChunk

-- | Combine a stream of unwrapped chunks into a PNG data stream.
chunksToPng :: Monad m => ConduitT PngChunk ByteString m ()
chunksToPng = do
  sourceLazy pngMagicString

  C.map putChunk .| conduitPut

-- | Query the dimensions of a PNG image.
pngDimensions :: MonadThrow m => ConduitT ByteString o m (Word32, Word32)
pngDimensions =
  pngToChunks
  .| C.filter (chunkType .== "IHDR")
  .| mapC (BS.toStrict . chunkData) -- TODO: I think this can be better.
  .| sinkGet getIhdrDimensions

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

-- | Parse a textual PNG chunk with the given keyword and yields its content.
parseKeywordChunks :: MonadThrow m => ByteString -> ConduitT PngChunk ByteString m ()
parseKeywordChunks key = awaitForever $ \PngChunk{..} -> do
  parser <- case chunkType of
    "tEXt" -> pure getTextChunk
    "zTXt" -> pure getZtxtChunk
    "iTXt" -> pure getItxtChunk
    _      -> throwM $ ParseException $
      "expected tEXt, zTXt, or iTXt chunk, but got " <> show chunkType

  case runGetOrFail (parser key) chunkData of
    Right ( _,      _, res) -> sourceLazy res
    Left  (bs, offset, err) -> throwM $ ParseError (BS.toStrict bs) offset err

parseVersionChunks :: MonadThrow m => ConduitT PngChunk Void m ByteString
parseVersionChunks =
  C.filter (isKeywordChunk "version")
  .| parseKeywordChunks "version"
  .| sinkExactly1

renderVersionChunk :: ByteString -> PngChunk
renderVersionChunk version = PngChunk
  { chunkType = "tEXt"
  , chunkData = runPut $ putTextChunk "version" (BS.fromStrict version)
  }

parseSettingChunks :: MonadThrow m => ConduitT PngChunk Void m Document
parseSettingChunks =
  C.filter (isKeywordChunk "preset")
  .| parseKeywordChunks "preset"
  .| sinkDoc def

renderSettingChunk :: Document -> PngChunk
renderSettingChunk doc = PngChunk
  { chunkType = "zTXt"
  , chunkData = runPut $ putZtxtChunk "preset" $ renderLBS def doc
  }

parseRegularChunks :: MonadThrow m => ConduitT PngChunk Void m BL.ByteString
parseRegularChunks =
  C.filter isRegularChunk
  .| chunksToPng
  .| sinkLazy
