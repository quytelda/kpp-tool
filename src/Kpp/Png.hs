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
module Kpp.Png
  ( PngChunk(..)
    -- * Binary parsing & rendering
    -- ** Chunk parsers
  , getChunk
  , putChunk
  , isSpecialChunk

    -- *** Textual chunks
  , getTextChunk
  , putTextChunk
  , getZtxtChunk
  , putZtxtChunk
  , getItxtChunk
  , putItxtChunk

    -- *** Other chunks
  , getIhdrDimensions

    -- *** KPP-specific chunks
  , ParsedChunk(..)
  , parseChunk
  , renderVersionChunk
  , renderSettingChunk

    -- * Conduits
  , unwrapChunks
  , wrapChunks
  , pngDimensions
  ) where

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

putNull :: Put
putNull = putWord8 0

data PngChunk = PngChunk
  { chunkType :: !ByteString
  , chunkData :: !BL.ByteString
  } deriving (Eq, Show)

chunkCRC :: PngChunk -> Word32
chunkCRC PngChunk{..} = crc32Update (crc32 chunkType) chunkData

isSpecialChunk :: PngChunk -> Bool
isSpecialChunk PngChunk{..} = isTextual && hasSpecialKey
  where
    isTextual =
      chunkType == "tEXt" ||
      chunkType == "zTXt" ||
      chunkType == "iTXt"
    hasSpecialKey =
      "version\0" `BL.isPrefixOf` chunkData ||
      "preset\0"  `BL.isPrefixOf` chunkData

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
getTextChunk :: Get (BL.ByteString, BL.ByteString)
getTextChunk = (,)
  <$> getLazyByteStringNul
  <*> getRemainingLazyByteString

-- | Build a tEXt chunk from a given key and value.
putTextChunk :: ByteString -> BL.ByteString -> Put
putTextChunk key value = do
  putByteString key *> putNull
  putLazyByteString value

-- | Parse a zTXt chunk with a matching key and return its decompressed content.
getZtxtChunk :: Get (BL.ByteString, BL.ByteString)
getZtxtChunk = (,)
  <$> getLazyByteStringNul
  <*  getWord8 -- compression type is always 0
  <*> (decompress <$> getRemainingLazyByteString)

-- | Build a zTXt chunk from a given key and value.
putZtxtChunk :: ByteString -> BL.ByteString -> Put
putZtxtChunk key value = do
  putByteString key *> putNull
  putWord8 0 -- compression type is always 0
  putLazyByteString $ compress value

-- | Parse a iTXt chunk with a matching key and return its decompressed content.
--
-- The language tag and translated keyword fields are ignored.
getItxtChunk :: Get (BL.ByteString, BL.ByteString)
getItxtChunk = do
  key <- getLazyByteStringNul
  compressed <- get :: Get Bool
  void getWord8 -- compression type is always 0
  void getLazyByteStringNul -- ignore language tag
  void getLazyByteStringNul -- ignore translated keyword
  content <- getRemainingLazyByteString

  return (key, if compressed
               then decompress content
               else content)

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

-- | Extract the width and height of an image from an IHDR chunk.
getIhdrDimensions :: Get (Word32, Word32)
getIhdrDimensions = do
  width  <- getWord32be
  height <- getWord32be
  return (width, height)

--------------------------------------------------------------------------------
-- KPP-specific Chunks

-- | A temporary sum type for the results of parsing a 'PngChunk'.
data ParsedChunk
  = VersionChunk BL.ByteString -- ^ Contains KPP version information
  | SettingChunk BL.ByteString -- ^ Contains preset settings document
  | IgnoredChunk PngChunk      -- ^ Regular PNG chunks with no KPP-specific purpose
  deriving (Eq, Show)

-- | Attempt to parse a 'PngChunk' into a special KPP-specific (i.e. a
-- version or settings chunk). If this fails, we classify the chunk as
-- ignored.
parseChunk :: PngChunk -> ParsedChunk
parseChunk chunk@PngChunk{..} =
  flip runGet chunkData $ getSpecialChunk <|> pure (IgnoredChunk chunk)
  where
    getSpecialChunk = do
      (key, val) <- case chunkType of
        "tEXt" -> getTextChunk
        "zTXt" -> getZtxtChunk
        "iTXt" -> getItxtChunk
        _      -> empty

      case key of
        "version" -> pure $ VersionChunk val
        "preset"  -> pure $ SettingChunk val
        _         -> empty

renderVersionChunk :: ByteString -> PngChunk
renderVersionChunk version = PngChunk
  { chunkType = "tEXt"
  , chunkData = runPut $ putTextChunk "version" (BS.fromStrict version)
  }

renderSettingChunk :: Document -> PngChunk
renderSettingChunk doc = PngChunk
  { chunkType = "zTXt"
  , chunkData = runPut $ putZtxtChunk "preset" $ renderLBS def doc
  }

--------------------------------------------------------------------------------
-- Conduits

-- | Divide a PNG data stream into a stream of unwrapped chunks.
unwrapChunks :: MonadThrow m => ConduitT ByteString PngChunk m ()
unwrapChunks = do
  -- Every PNG starts with the same 8-byte magic string.
  magic <- takeCE 8 .| sinkLazy
  unless (magic == pngMagicString) $
    throwM $ ParseException "invalid PNG data"

  conduitGet getChunk

-- | Combine a stream of unwrapped chunks into a PNG data stream.
wrapChunks :: Monad m => ConduitT PngChunk ByteString m ()
wrapChunks = do
  sourceLazy pngMagicString

  C.map putChunk .| conduitPut

-- | Query the dimensions of a PNG image.
pngDimensions :: MonadThrow m => ConduitT ByteString o m (Word32, Word32)
pngDimensions = do
  unwrapChunks .| C.head >>= \case
    Just (PngChunk "IHDR" content) ->
      sourceLazy content
      .| sinkGet getIhdrDimensions
    _ -> throwM $ ParseException "expected IDHR chunk"
