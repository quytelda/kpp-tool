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
  , getParam
  , describeResource
  , describeParam
  , describePreset
  ) where

import           Codec.Picture
import qualified Codec.Picture.Metadata as Meta
import           Codec.Picture.Png      (decodePngWithMetadata)
import           Control.Applicative
import           Data.Bifunctor         (first)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.ByteString.Base64
import qualified Data.ByteString.Lazy   as BL
import           Data.Foldable          (toList)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.String
import qualified Data.Text              as T
import           Data.Text.Encoding     (encodeUtf8)
import qualified Data.Text.Lazy         as TL
import           Data.Text.Read         (decimal)
import           Text.XML
import           Text.XML.Cursor

-- | Generalized version of show that works for Text and ByteString.
show_ :: (IsString c, Show a) => a -> c
show_ = fromString . show

toEither :: Foldable t => a -> t b -> Either a b
toEither = foldr (const . Right) . Left

-- | Equivalent to attributeText from Text.XML.Types for xml-conduit
attributeText :: Name -> Element -> Maybe T.Text
attributeText name = Map.lookup name . elementAttributes

-- | Decode binary data encoded in base64 text.
decodeBinary :: T.Text -> [ByteString]
decodeBinary = toList . decodeBase64 . encodeUtf8

-- | Parse an Int from a Text representation.
--
-- The integer must contain only the digits 0-9 with no spaces.
parseInt :: Alternative f => T.Text -> f Int
parseInt text = case decimal text of
  Right (n, "") -> pure n
  _             -> empty

-- | Encode a DynamicImage as a PNG with provided metadata (if possible).
--
-- JuicyPixels doesn't currently provide a function to encode a
-- DynamicImage with metadata information.
encodeDynamicPngWithMetadata :: Meta.Metadatas -> DynamicImage -> Either String BL.ByteString
encodeDynamicPngWithMetadata meta (ImageRGB8 img)   = Right $ encodePngWithMetadata meta img
encodeDynamicPngWithMetadata meta (ImageRGBA8 img)  = Right $ encodePngWithMetadata meta img
encodeDynamicPngWithMetadata meta (ImageRGB16 img)  = Right $ encodePngWithMetadata meta img
encodeDynamicPngWithMetadata meta (ImageRGBA16 img) = Right $ encodePngWithMetadata meta img
encodeDynamicPngWithMetadata meta (ImageY8 img)     = Right $ encodePngWithMetadata meta img
encodeDynamicPngWithMetadata meta (ImageY16 img)    = Right $ encodePngWithMetadata meta img
encodeDynamicPngWithMetadata meta (ImageYA8 img)    = Right $ encodePngWithMetadata meta img
encodeDynamicPngWithMetadata meta (ImageYA16 img)   = Right $ encodePngWithMetadata meta img
encodeDynamicPngWithMetadata _ _                    = Left "Unsupported image format for PNG export"

-- | Resource is a type for embedded resources.
data Resource = Resource { resourceName :: !T.Text
                         , resourceFile :: !T.Text
                         , resourceType :: !T.Text
                         , resourceCsum :: !T.Text
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

-- | Pretty print a description of a resource.
describeResource :: Resource -> T.Text
describeResource Resource{..} = T.unlines $
  [ "Name: " <> resourceName
  , "Path: " <> resourceFile
  , "Type: " <> resourceType
  , "MD5: "  <> resourceCsum
  , "Data: " <> showBinary resourceData
  ]

-- | Render a Resource into an XML element.
--
-- Since this contains base64-encoded binary data, the resulting
-- element content can be fairly long.
resourceToXML :: Resource -> Node
resourceToXML Resource{..} =
  let elementName       = "resource"
      elementNodes      = [NodeContent $ encodeBase64 resourceData]
      elementAttributes = Map.fromList [ ("name",     resourceName)
                                       , ("filename", resourceFile)
                                       , ("type",     resourceType)
                                       , ("md5sum",   resourceCsum)
                                       ]
  in NodeElement Element{..}

-- | Select resource elements and parse them into Resource structures.
--
-- Any content is appropriately decoded from base64 into byte data.
resources :: Cursor -> [(T.Text, Resource)]
resources cursor = do
  resourceName <- attribute "name" cursor
  resourceType <- attribute "type" cursor
  resourceFile <- attribute "filename" cursor
  resourceCsum <- attribute "md5sum" cursor
  resourceData <- descendant cursor >>= content >>= decodeBinary

  return (resourceName, Resource{..})

-- | A Param represents the value of a preset parameter.
--
-- This value has a type, which can be "string" (for textual data) or
-- "bytearray" (for binary data, encoded in base64).
data Param = String !T.Text
           | Binary !ByteString

-- | Convert a ByteString to a short String for display purposes. Long
-- ByteStrings are truncated.
showBinary :: (Semigroup s, IsString s) => ByteString -> s
showBinary bytes
  | size <= maxlen = show_ bytes
  | otherwise      = show_ preview
                     <> "... ("
                     <> show_ size
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

describeParam :: T.Text -> Param -> T.Text
describeParam name (String text)  = name <> ": " <> text
describeParam name (Binary bytes) = name <> ": " <> showBinary bytes

-- | Helper function to construct <param> elements.
paramElement :: T.Text -> T.Text -> T.Text -> Node
paramElement paramName paramType paramData =
  let elementName       = "param"
      elementNodes      = [NodeContent paramData]
      elementAttributes = Map.fromList [ ("name", paramName)
                                       , ("type", paramType)
                                       ]
  in NodeElement Element{..}

-- | Render a Param to an XML element representation.
paramToXML :: T.Text -> Param -> Node
paramToXML paramName (String text)  = paramElement paramName "string" text
paramToXML paramName (Binary bytes) = paramElement paramName "bytearray" $ encodeBase64 bytes

-- | Select parameter elements and parse them into Param tables.
--
-- I'm not sure why, but some binary data in KPP parameters might be
-- base64-encoded twice; the issue seems to apply to parameter values
-- in both versions 2.2 and 5.0, but is not consistent.
params :: Cursor -> [(T.Text, Param)]
params cursor = do
  paramName <- attribute "name" cursor
  paramType <- attribute "type" cursor
  paramData <- descendant cursor >>= content

  paramValue <- case paramType of
    "string"    -> String <$> return paramData
    "bytearray" -> Binary <$> decodeBinary' paramData
    _           -> mempty

  return (paramName, paramValue)
  where
    decodeBinary' text = do
      bin <- decodeBinary text
      if isBase64 bin
        then toList $ decodeBase64 bin
        else return bin

-- | Preset represents a Krita brush preset.
data Preset = Preset { presetName        :: !T.Text
                     , presetPaintop     :: !T.Text
                     , presetVersion     :: !T.Text
                     , presetParams      :: Map T.Text Param
                     , embeddedResources :: Map T.Text Resource
                     , presetIcon        :: (DynamicImage, Meta.Metadatas)
                     }

presetIconDims :: Preset -> (Int, Int)
presetIconDims Preset{presetIcon = (icon, _)} =
  let iconWidth  = dynamicMap imageWidth  icon
      iconHeight = dynamicMap imageHeight icon
  in (iconWidth, iconHeight)

-- | Preset records contain image data, so we provide a custom
-- instance for Show that describes images in terms of dimensions.
instance Show Preset where
  show preset@Preset{..} =
    unwords [ "Preset"
            , show presetName
            , show presetPaintop
            , show presetVersion
            , show presetParams
            , show embeddedResources
            , "[" ++ show iconWidth ++ "x" ++ show iconHeight ++ "]"
            ]
    where
      (iconWidth, iconHeight) = presetIconDims preset

describePreset :: Preset -> T.Text
describePreset preset@Preset{..} = T.unlines $
  [ "Name: "    <> presetName
  , "Type: "    <> presetPaintop
  , "Version: " <> presetVersion
  , "Icon: "    <> show_ (presetIconDims preset)
  , "\nParameters:"
  ]
  <>
  map indent paramList
  <>
  [ "\nResources: " ]
  <>
  resourceList
  where
    indent = ("  " <>)
    paramList    = Map.elems $ Map.mapWithKey describeParam presetParams
    resourceList = Map.elems $ Map.map describeResource embeddedResources

-- | Generate a <resources> element containing a list of <resource> elements.
resourcesToXML :: Map T.Text Resource -> Node
resourcesToXML = NodeElement . Element "resources" mempty . fmap resourceToXML . Map.elems

-- | Generate a preset settings XML Document for a Preset.
presetToXML :: Preset -> Document
presetToXML Preset{..} =
  let prologue = Prologue [] Nothing []
      epilogue = []

      -- Root <Preset> Element
      resourceCount     = show_ $ Map.size embeddedResources
      paramElems        = Map.elems $ Map.mapWithKey paramToXML presetParams
      elementName       = "Preset"
      elementAttributes = Map.fromList [ ("name",               presetName)
                                       , ("paintopid",          presetPaintop)
                                       , ("embedded_resources", resourceCount)
                                       ]
      elementNodes      = resourcesToXML embeddedResources : paramElems
  in Document prologue Element{..} epilogue

-- | Decode binary KPP file data (PNG data) into a Preset.
decodeKPP :: ByteString -> Either String Preset
decodeKPP bytes = do
  presetIcon@(_, meta) <- decodePngWithMetadata bytes

  presetVersion <- getVersion meta
  doc           <- getSettings meta >>= parseSettings

  let root = documentRoot doc
  presetName    <- getName    root
  presetPaintop <- getPaintop root

  let cursor            = fromDocument doc
      presetParams      = Map.fromList $ cursor $/ params
      embeddedResources = Map.fromList $ cursor $/ element "resources" &/ resources
      resourceCount     = attributeText "embedded_resources" root >>= parseInt

  case resourceCount of
    Just n | n /= Map.size embeddedResources -> Left "embedded resource count mismatch"
    _                                        -> return ()

  return Preset{..}
  where
    getName    = toEither "missing preset name"      . attributeText "name"
    getPaintop = toEither "missing preset paintopid" . attributeText "paintopid"
    parseSettings = first show . parseText def

-- | Encode a Preset as binary PNG data.
encodeKPP :: Preset -> Either String BL.ByteString
encodeKPP preset@Preset{presetIcon = (icon, meta)} =
  let renderSettings = def { rsUseCDATA = const True }
      xml = renderText renderSettings $ presetToXML preset
      meta' = setSettings meta xml
  in encodeDynamicPngWithMetadata meta' icon

-- | This is the metadata key for looking up the preset settings XML
-- in the PNG metadata.
presetSettingsKey :: Meta.Keys Meta.Value
presetSettingsKey = Meta.Unknown "preset"

-- | This is the version key for looking up the preset version.
presetVersionKey :: Meta.Keys Meta.Value
presetVersionKey = Meta.Unknown "version"

-- | Locate the preset version from the PNG metadata table.
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

-- | Lookup the value of a preset parameter.
getParam :: T.Text -> Preset -> Maybe Param
getParam key = Map.lookup key . presetParams
