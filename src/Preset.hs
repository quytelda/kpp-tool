{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Preset where

import           Codec.Picture
import           Codec.Picture.Metadata
import           Data.ByteString        (ByteString)
import           Data.Map.Strict        (Map)
import           Data.Text              (Text)

-- | A Param represents the value of a preset parameter.
--
-- This value has a type, which can be "string" (for textual data) or
-- "bytearray" (for binary data, encoded in base64).
data Param = String !Text
           | Binary !ByteString
           deriving (Show)

-- | Preset represents a Krita brush preset.
data Preset = Preset { presetName    :: !Text
                     , presetPaintop :: !Text
                     , presetParams  :: Map Text Param
                     , presetIcon    :: (DynamicImage, Metadatas)
                     }

-- | Decode binary KPP file data (PNG data) into a Preset.
decodeKPP :: ByteString -> Either String Preset
decodeKPP bytes = undefined
  -- 1. Decode PNG data
  -- 2. Lookup settings XML in metadata
  -- 3. Parse settings XML

-- | Encode a Preset as binary PNG data.
encodeKPP :: Preset -> Either String ByteString
encodeKPP preset = undefined
  -- 1. Render settings XML
  -- 2. Insert settings into metadata
  -- 3. Encode PNG data

-- | Locate the preset settings from the PNG metadata table.
getSettings :: Metadatas -> Text
getSettings = undefined

-- | Parse preset settings XML.
parseSettings :: Text -> DynamicImage -> Metadatas -> Preset
parseSettings = undefined
