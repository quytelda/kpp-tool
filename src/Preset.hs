{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Preset
  ( Param
  , Preset(..)
  , Resource(..)
  , decodeKPP
  , encodeKPP
  , getSettings
  , parseSettings
  ) where

import           Codec.Picture
import           Codec.Picture.Metadata
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.Map.Strict        (Map)
import           Data.Text              (Text)

-- | Resource is a type for embedded resources.
data Resource = Resource { resourceName :: !Text
                         , resourceFile :: !Text
                         , resourceType :: !Text
                         , resourceCsum :: !Text
                         , resourceData :: !ByteString
                         }

-- | Resource records often contain really long binary strings, so we
-- provide a custom abbreviated instance for Show.
instance Show Resource where
  show Resource{..} =
    unwords [ "Resource"
            , show resourceName
            , show resourceFile
            , show resourceType
            , show resourceCsum
            , show (BS.length resourceData) ++ "b"
            ]

-- | A Param represents the value of a preset parameter.
--
-- This value has a type, which can be "string" (for textual data) or
-- "bytearray" (for binary data, encoded in base64).
data Param = String !Text
           | Binary !ByteString
           deriving (Show)

-- | Preset represents a Krita brush preset.
data Preset = Preset { presetName        :: !Text
                     , presetPaintop     :: !Text
                     , presetParams      :: Map Text Param
                     , embeddedResources :: [Resource]
                     , presetIcon        :: (DynamicImage, Metadatas)
                     }

-- | Decode binary KPP file data (PNG data) into a Preset.
decodeKPP :: ByteString -> Either String Preset
decodeKPP = undefined
  -- 1. Decode PNG data
  -- 2. Lookup settings XML in metadata
  -- 3. Parse settings XML

-- | Encode a Preset as binary PNG data.
encodeKPP :: Preset -> Either String ByteString
encodeKPP = undefined
  -- 1. Render settings XML
  -- 2. Insert settings into metadata
  -- 3. Encode PNG data

-- | Locate the preset settings from the PNG metadata table.
getSettings :: Metadatas -> Text
getSettings = undefined

-- | Parse preset settings XML.
parseSettings :: Text -> DynamicImage -> Metadatas -> Preset
parseSettings = undefined
