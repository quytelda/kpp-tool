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
module Kpp.PngConduit where

import           Codec.Compression.Zlib
import           Conduit
import           Control.Applicative
import           Control.Exception
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
import qualified Kpp.Png                           as Png

data ParseException = ParseException
  deriving (Eq, Show)

instance Exception ParseException

-- | Parse a single Chunk from a PNG data stream.
--
-- Fail if the checksum verification is unsuccessful.
getPngChunk :: Get ByteString
getPngChunk = do
  chunkLength <- getWord32be
  chunkData   <- getByteString $ fromIntegral (4 + chunkLength)
  chunkCsum   <- getWord32be

  if chunkCsum == crc32 chunkData
    then pure chunkData
    else fail "checksum mismatch"

putPngChunk :: ByteString -> Put
putPngChunk chunkData = do
  putWord32be   chunkLength
  putByteString chunkData
  putWord32be   chunkCsum
  where
    chunkLength = fromIntegral $ BS.length chunkData - 4
    chunkCsum   = crc32 chunkData

pngMagic :: BL.ByteString
pngMagic = "\x89\x50\x4E\x47\x0D\x0A\x1A\x0A"

pngToChunks :: MonadThrow m => ConduitT ByteString ByteString m ()
pngToChunks = do
  magic <- takeCE 8 .| sinkLazy
  unless (magic == pngMagic) $
    throwM ParseException

  conduitGet getPngChunk

chunksToPng :: Monad m => ConduitT ByteString ByteString m ()
chunksToPng = do
  yield (BS.toStrict pngMagic)
  C.map putPngChunk
    .| conduitPut

unchunk :: [BL.ByteString] -> BL.ByteString
unchunk cs = runConduitPure $ yieldMany cs .| C.map BS.toStrict .| chunksToPng .| sinkLazy

tochunks :: MonadThrow m => BL.ByteString -> m [BL.ByteString]
tochunks png = runConduit $ sourceLazy png .| pngToChunks .| C.map BS.fromStrict .| sinkList

isKeywordChunk :: ByteString -> ByteString -> Bool
isKeywordChunk key bs =
  t `elem` ["tEXt", "zTXt", "iTXt"]
  && (BS.append key "\0") `BS.isPrefixOf` bs'
  where (t, bs') = BS.splitAt 4 bs

isRegularChunk :: ByteString -> Bool
isRegularChunk bs = not $
  isKeywordChunk "version" bs || isKeywordChunk "preset" bs

sinkExactlyOne :: MonadThrow m => ConduitT a Void m a
sinkExactlyOne = do
  r <- await >>= maybe (throwM ParseException) pure

  done <- C.null
  unless done $
    throwM ParseException
  return r

sinkExactly1 :: MonadThrow m => ConduitT a Void m a
sinkExactly1 = await <* endOfInput >>= maybe (throwM ParseException) pure
  where
    endOfInput = do
      done <- C.null
      unless done $
        throwM ParseException

parseKeywordChunks :: MonadThrow m => ByteString -> ConduitT ByteString ByteString m ()
parseKeywordChunks key =
  C.filter (isKeywordChunk key)
  .| conduitGet (BS.toStrict <$> Png.getKeywordChunk (BS.fromStrict key))

handleVersionChunks :: MonadThrow m => ConduitT ByteString Void m ByteString
handleVersionChunks =
  parseKeywordChunks "version"
  .| sinkExactly1

handleSettingChunks :: MonadThrow m => ConduitT ByteString Void m Document
handleSettingChunks =
  parseKeywordChunks "preset"
  .| sinkDoc def

handleRegularChunks :: MonadThrow m => ConduitT ByteString Void m BL.ByteString
handleRegularChunks =
  C.filter isRegularChunk
  .| chunksToPng
  .| sinkLazy

parseChunks :: MonadThrow m => ConduitT ByteString Void m (ByteString, Document, BL.ByteString)
parseChunks = getZipSink $
  (,,)
  <$> ZipSink handleVersionChunks
  <*> ZipSink handleSettingChunks
  <*> ZipSink handleRegularChunks
