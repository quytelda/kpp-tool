{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Preset
  ( Param
  , Preset(..)
  , Resource(..)
  , decodeKPP
  , encodeKPP
  , getSettings
  , setSettings
  , parseSettings
  ) where

import           Codec.Picture
import qualified Codec.Picture.Metadata as Meta
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.ByteString.Base64 (decodeBase64)
import           Data.Map.Strict        (Map)
import           Data.Text              (Text)
import           Data.Text.Encoding     (encodeUtf8)
import qualified Data.Text.Lazy         as TL

eitherToList :: Either a b -> [b]
eitherToList = either (const mempty) pure

-- | Decode binary data encoded in base64 text.
decodeBinary :: Text -> [ByteString]
decodeBinary = eitherToList . decodeBase64 . encodeUtf8

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
                     , presetIcon        :: (DynamicImage, Meta.Metadatas)
                     }

-- | Preset records contain image data, so we provide a custom
-- instance for Show that describes images in terms of dimensions.
instance Show Preset where
  show Preset{..} =
    unwords [ "Preset"
            , show presetName
            , show presetPaintop
            , show presetParams
            , show embeddedResources
            , "[" ++ show iconWidth ++ "x" ++ show iconHeight ++ "]"
            ]
    where
      iconWidth  = dynamicMap imageWidth  (fst presetIcon)
      iconHeight = dynamicMap imageHeight (fst presetIcon)

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

presetSettingsKey :: Meta.Keys Meta.Value
presetSettingsKey = Meta.Unknown "preset"

-- | Locate the preset settings from the PNG metadata table.
getSettings :: Meta.Metadatas -> Either String TL.Text
getSettings meta =
  case Meta.lookup presetSettingsKey meta of
    Just (Meta.String s) -> Right (TL.pack s)
    _                    -> Left "Missing or invalid preset metadata"

-- | Insert preset settings XML into a PNG metadata table.
setSettings :: Meta.Metadatas -> TL.Text -> Meta.Metadatas
setSettings meta xml = Meta.insert presetSettingsKey value meta
  where value = Meta.String $ TL.unpack xml

-- | Parse preset settings XML.
parseSettings :: Text -> DynamicImage -> Meta.Metadatas -> Preset
parseSettings = undefined
