{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Preset
  ( Param(..)
  , Preset(..)
  , Resource(..)
  , decodeKPP
  , encodeKPP
  , getSettings
  , setSettings
  ) where

import           Codec.Picture
import qualified Codec.Picture.Metadata as Meta
import           Codec.Picture.Png      (decodePngWithMetadata)
import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor         (first)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.ByteString.Base64 (decodeBase64)
import           Data.Foldable          (toList)
import           Data.Function          ((&))
import           Data.Functor           ((<&>))
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding     (encodeUtf8)
import qualified Data.Text.Lazy         as TL
import           Data.Text.Read         (decimal)
import           Text.XML
import           Text.XML.Cursor

listToEither :: String -> [b] -> Either String b
listToEither = foldr ((<>) . Right) . Left

-- | Decode binary data encoded in base64 text.
decodeBinary :: Text -> [ByteString]
decodeBinary = toList . decodeBase64 . encodeUtf8

-- | Parse an Int from a Text representation.
--
-- The integer must contain only the digits 0-9 with no spaces.
parseInt :: Alternative f => Text -> f Int
parseInt text = case decimal text of
  Right (n, "") -> pure n
  _             -> empty

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
            , showBinary resourceData
            ]

-- | Select resource elements and parse them into Resource structures.
--
-- Any content is appropriately decoded from base64 into byte data.
resources :: Cursor -> [Resource]
resources cursor = do
  resourceName <- attribute "name" cursor
  resourceType <- attribute "type" cursor
  resourceFile <- attribute "filename" cursor
  resourceCsum <- attribute "md5sum" cursor
  resourceData <- descendant cursor >>= content >>= decodeBinary

  return $ Resource{..}

-- | A Param represents the value of a preset parameter.
--
-- This value has a type, which can be "string" (for textual data) or
-- "bytearray" (for binary data, encoded in base64).
data Param = String !Text
           | Binary !ByteString

-- | Convert a ByteString to a short String for display purposes. Long
-- ByteStrings are truncated.
showBinary :: ByteString -> String
showBinary bytes
  | size <= maxlen = show bytes
  | otherwise      = show preview
                     <> "... (+"
                     <> show (size - maxlen)
                     <> " bytes)"
    where
      size    = BS.length bytes
      maxlen  = 16
      preview = BS.take maxlen bytes

-- | Binary parameters are often quite long, so this custom Show
-- instance truncates the displayed ByteString if the input is too
-- long.
instance Show Param where
  show (String text)  = "String " <> show text
  show (Binary bytes) = "Binary " <> showBinary bytes

-- | Select parameter elements and parse them into Param tables.
params :: Cursor -> [(Text, Param)]
params cursor = do
  paramName <- attribute "name" cursor
  paramType <- attribute "type" cursor
  paramData <- descendant cursor >>= content

  paramValue <- case paramType of
    "string"    -> String <$> return paramData
    "bytearray" -> Binary <$> decodeBinary paramData
    _           -> mempty

  return (paramName, paramValue)

-- | Preset represents a Krita brush preset.
data Preset = Preset { presetName        :: !Text
                     , presetPaintop     :: !Text
                     , presetVersion     :: !Text
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
            , show presetVersion
            , show presetParams
            , show embeddedResources
            , "[" ++ show iconWidth ++ "x" ++ show iconHeight ++ "]"
            ]
    where
      iconWidth  = dynamicMap imageWidth  (fst presetIcon)
      iconHeight = dynamicMap imageHeight (fst presetIcon)

-- | Decode binary KPP file data (PNG data) into a Preset.
decodeKPP :: ByteString -> Either String Preset
decodeKPP bytes = do
  presetIcon@(_, meta) <- decodePngWithMetadata bytes

  presetVersion <- getVersion meta
  cursor        <- getSettings meta >>= parseSettings <&> fromDocument

  let presetParams      = (cursor $/ params) & Map.fromList
      embeddedResources = cursor $/ element "resources" &/ resources

  listToEither "invalid preset settings" $ do
    presetName    <- attribute "name" cursor
    presetPaintop <- attribute "paintopid" cursor
    resourceCount <- attribute "embedded_resources" cursor >>= parseInt

    -- Sanity check: were the expected number of resources loaded?
    guard (length embeddedResources == resourceCount)

    return Preset{..}
  where
    parseSettings = first show . parseText def

-- | Encode a Preset as binary PNG data.
encodeKPP :: Preset -> Either String ByteString
encodeKPP = undefined
  -- 1. Render settings XML
  -- 2. Insert settings into metadata
  -- 3. Encode PNG data

-- | This is the metadata key for looking up the preset settings XML
-- in the PNG metadata.
presetSettingsKey :: Meta.Keys Meta.Value
presetSettingsKey = Meta.Unknown "preset"

-- | This is the version key for looking up the preset version.
presetVersionKey :: Meta.Keys Meta.Value
presetVersionKey = Meta.Unknown "version"

getVersion :: Meta.Metadatas -> Either String T.Text
getVersion meta =
  case Meta.lookup presetVersionKey meta of
    Just (Meta.String s) -> Right (T.pack s)
    _                    -> Left "missing or invalid preset version"

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
