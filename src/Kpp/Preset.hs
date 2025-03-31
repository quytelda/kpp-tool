{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : Kpp.Preset
Copyright   : (c) Quytelda Kahja, 2024
License     : BSD-3-Clause

This module contains functions and data structures for parsing,
rendering, and manipulating brush presets (KPP files).
-}
module Kpp.Preset
  ( parseSettingsXml
  , FilterConfig(..)
  , Resource(..)
  , resourceMD5
  , prettyResources
  , loadResource
  , saveResource
  , Preset(..)
  , loadPreset
  , savePreset
  , setPresetName
  , lookupParam
  , insertParam
  , lookupResourceByName
  , lookupResourceByFile
  , lookupResourceByMD5
  , insertResource
  , getPresetIcon
  , setPresetIcon
  , presetIconDimensions
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import qualified Crypto.Hash.MD5        as MD5
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString        as BS
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.ByteString.Lazy   as BL
import           Data.Foldable
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Prettyprinter          hiding (width)
import           System.FilePath
import           Text.XML

import           Kpp.Common
import           Kpp.Param
import           Kpp.Png

-------------------------------------------
-- Preset (KPP file) Parsing & Rendering --
-------------------------------------------

-- | `FilterConfig` represents the serialized settings for a filter.
--
-- @<filterconfig>@ elements appears in filter preset settings and
-- contain a list of parameters. Acceptable versions seem to be "1"
-- and "2" as of Dec. 2024.
--
-- Note: Some parameters might be missing the type attribute.
data FilterConfig = FilterConfig
  { filterVersion :: !Text
  , filterParams  :: !(Map Text ParamValue)
  } deriving (Eq, Show)

instance Pretty FilterConfig where
  pretty FilterConfig{..} =
    parens ("version=" <> viaShow filterVersion)
    <\> prettyParams filterParams

parseXml_filterconfig :: MonadError String m => Element -> m FilterConfig
parseXml_filterconfig = withElement "filterconfig" $ \e-> do
  filterVersion <- attributeText "version" e
  filterParams  <- Map.fromList <$> traverse parseXml_param (childElements e)
  return FilterConfig{..}

renderXml_filterconfig :: FilterConfig -> Element
renderXml_filterconfig FilterConfig{..} =
  let elementName       = "filterconfig"
      elementNodes      = NodeElement <$> renderXml_params filterParams
      elementAttributes = Map.fromList [ ("version", filterVersion) ]
  in Element{..}


-- | 'Resource' is a type for embedded resources.
data Resource = Resource { resourceName :: !Text
                         , resourceFile :: !Text
                         , resourceType :: !Text
                         , resourceData :: !BS.ByteString
                         } deriving (Eq, Show)

instance Pretty Resource where
  pretty Resource{..} =
    vsep [ "name:" <+> pretty resourceName
         , "file:" <+> pretty resourceFile
         , "type:" <+> pretty resourceType
         , "data:" <+> prettyByteData resourceData
         , "md5:"  <+> pretty (md5sum resourceData)
         ]

-- | Calculate the MD5 checksum of a `Resource` displayed in
-- hexadecimal notation.
resourceMD5 :: Resource -> Text
resourceMD5 = md5sum . resourceData

-- | Load a resource file.
loadResource :: FilePath -> Text -> Maybe Text -> Maybe Text -> IO Resource
loadResource path resourceType mname mfile = do
  let resourceFile = fromMaybe (T.pack $ takeFileName path) mfile
      resourceName = fromMaybe (T.pack $ takeFileName path) mname
  resourceData <- BS.readFile path
  return Resource{..}

-- | Save a resource, returning the path used.
saveResource :: Maybe FilePath -> Resource -> IO FilePath
saveResource mpath Resource{..} = do
  let path = fromMaybe (T.unpack resourceFile) mpath
  BS.writeFile path resourceData
  return path

parseXml_resource :: MonadError String m => Element -> m Resource
parseXml_resource = withElement "resource" $ \e -> do
  resourceName <- attributeText "name"     e
  resourceType <- attributeText "type"     e
  resourceFile <- attributeText "filename" e
  resourceCsum <- attributeText "md5sum"   e >>= decodeBase16
  resourceData <- contentText              e >>= decodeBase64

  -- verify checksum
  if resourceCsum == MD5.hash resourceData
    then pure Resource{..}
    else throwError $ "checksum mismatch for resource: " <> show resourceName

renderXml_resource :: Resource -> Element
renderXml_resource Resource{..} =
  let resourceCsum      = md5sum resourceData
      elementName       = "resource"
      elementNodes      = [NodeContent $ encodeBase64 resourceData]
      elementAttributes = Map.fromList [ ("name",     resourceName)
                                       , ("filename", resourceFile)
                                       , ("type",     resourceType)
                                       , ("md5sum",   resourceCsum)
                                       ]
  in Element{..}

-- | Parse a @<resources>@ XML element, which should contain a list of
-- all embedded resources.
parseXml_resources :: MonadError String m => Element -> m (Map Text Resource)
parseXml_resources = withElement "resources" $ \e -> do
  resources <- forM (childElements e) parseXml_resource
  return $ Map.fromList $ zip (resourceName <$> resources) resources

renderXml_resources :: Map Text Resource -> Element
renderXml_resources rs =
  let elementName       = "resources"
      elementNodes      = NodeElement <$> renderXml_resource <$> Map.elems rs
      elementAttributes = Map.empty
  in Element{..}

-- | A `Preset` represents a Krita brush preset, including its
-- settings and any embedded resources.
data Preset = Preset
  { presetVersion     :: !BS.ByteString
  , presetName        :: !Text
  , presetPaintop     :: !Text
  , presetParams      :: !(Map Text ParamValue)
  , presetFilter      :: !(Maybe FilterConfig)
  , embeddedResources :: !(Map Text Resource)
  , presetIcon        :: ![ByteString]
  } deriving (Eq, Show)

-- | Format an optional table of filter settings.
prettyFilter :: Maybe FilterConfig -> Doc ann
prettyFilter Nothing             = "None"
prettyFilter (Just filterConfig) = pretty filterConfig

-- | Format a table of embedded resource entries.
prettyResources :: Map Text Resource -> Doc ann
prettyResources m | null m    = "None"
                  | otherwise = concatWith (<\\>) $ pretty <$> m

instance Pretty Preset where
  pretty preset@Preset{..} =
    vsep [ "name:"    <+> pretty  presetName
         , "version:" <+> viaShow presetVersion
         , "paintop:" <+> pretty  presetPaintop
         , "icon:"    <+> pretty width <> "x" <> pretty height
         ]
    <\\> nest 2 ("Parameters:"          <\> prettyParams    presetParams)
    <\\> nest 2 ("Filter Settings:"     <\> prettyFilter    presetFilter)
    <\\> nest 2 ("Embedded Resources:"  <\> prettyResources embeddedResources)
    where
      (width, height) = presetIconDimensions preset

-- | Parse a @<Preset>@ XML element, which should be the root element
-- of the preset settings document.
parseXml_Preset :: MonadError String m => BS.ByteString -> [ByteString] -> Element -> m Preset
parseXml_Preset presetVersion presetIcon = withElement "Preset" $ \e -> do
  presetName    <- attributeText "name"      e
  presetPaintop <- attributeText "paintopid" e

  (presetParams, presetFilter, embeddedResources) <- foldM
    (\(params, filters, resources) child ->
        case elementName child of
          "param"        -> do
            (k,v) <- parseXml_param child
            pure (Map.insert k v params, filters, resources)
          "filterconfig" -> do
            filterConfig <- parseXml_filterconfig child
            if null filters
              then pure (params, Just filterConfig, resources)
              else throwError "found multiple <filterconfig> elements"
          "resources"    -> do
            resourceMap <- parseXml_resources child
            pure (params, filters, resourceMap <> resources)
          name           -> throwError $ "unrecognized element: " <> T.unpack (nameLocalName name)
    ) (mempty, empty, mempty) (childElements e)

  -- If an expected resource count is provided we check it for
  -- accuracy; otherwise we can only assume all resources are present.
  case Map.lookup "embedded_resources" (elementAttributes e) of
    Just val -> do
      resourceCount <- decodeInt val
      unless (resourceCount == Map.size embeddedResources) $
        throwError "resource count mismatch"
    Nothing -> pure ()

  return Preset{..}

-- | Generate an XML @<Preset>@ element.
renderXml_Preset :: Preset -> Element
renderXml_Preset Preset{..} =
  let paramNodes        = renderXml_params    presetParams
      resourcesNode     = renderXml_resources embeddedResources
      filterNodes       = renderXml_filterconfig <$> maybeToList presetFilter
      resourceCount     = T.pack $ show $ length embeddedResources
      elementName       = "Preset"
      elementNodes      = NodeElement <$> resourcesNode : paramNodes <> filterNodes
      elementAttributes = Map.fromList [ ("name",      presetName)
                                       , ("paintopid", presetPaintop)
                                       , ("embedded_resources", resourceCount)
                                       ]
  in Element{..}

getPreset :: Get Preset
getPreset = do
  getMagicString

  chunks <- some getChunk
  let versionChunks = [v | VersionChunk v <- chunks]
      settingChunks = [p | SettingChunk p <- chunks]
      regularChunks = [r | RegularChunk r <- chunks]

  when (null versionChunks) $
    fail "missing preset version chunk"

  when (null settingChunks) $
    fail "missing preset settings chunks"

  when (length versionChunks > 1 || length settingChunks > 1) $
    fail "duplicated metadata chunks"

  let version = BS.toStrict (head versionChunks)
      xml     = head settingChunks

  either fail pure
    $ parseXml_Preset version regularChunks
    $ documentRoot
    $ parseLBS_ def xml

putPreset :: Preset -> Put
putPreset preset@Preset{..} = do
  -- The metadata chunks must be inserted after the IHDR chunk.
  -- Krita inserts the elements following the IHDR and pHYs chunks,
  -- but before the IDAT chunks, so this code matches that behavior.
  let isFollower bs = BL.isPrefixOf "IDAT" bs || BL.isPrefixOf "IEND" bs
      (pre, post) = break isFollower presetIcon
      versionChunk = VersionChunk $ BS.fromStrict presetVersion
      settingChunk = SettingChunk $ makeSettingXml preset

  putMagicString
  traverse_ put (RegularChunk <$> pre)
  put settingChunk
  put versionChunk
  traverse_ put (RegularChunk <$> post)

instance Binary Preset where
  get = getPreset
  put = putPreset

-- | Read and parse a KPP file.
loadPreset :: FilePath -> IO Preset
loadPreset path = do
  contents <- BL.readFile path
  return $ decode contents

-- | Render and write a KPP file.
savePreset :: FilePath -> Preset -> IO ()
savePreset path preset =
  let contents = encode preset
  in BL.writeFile path contents

-- | Look up the value of a preset parameter.
lookupParam :: Text -> Preset -> Maybe ParamValue
lookupParam key = Map.lookup key . presetParams

-- | Insert or update a preset parameter.
insertParam :: Text -> ParamValue -> Preset -> Preset
insertParam key val preset@Preset{..} =
  preset { presetParams = Map.insert key val presetParams }

-- | Look up a resource by name, i.e. it's @name@ attribute.
lookupResourceByName :: Text -> Preset -> Maybe Resource
lookupResourceByName name = Map.lookup name . embeddedResources

-- | Look up a resource by its "filename" attribute (`resourceFile`).
lookupResourceByFile :: Text -> Preset -> Maybe Resource
lookupResourceByFile fileName = find hasFileName . embeddedResources
  where
    hasFileName Resource{..} = fileName == resourceFile

-- | Look up a resource by its MD5 checksum.
lookupResourceByMD5 :: Text -> Preset -> Maybe Resource
lookupResourceByMD5 md5 = find hasMatchingMD5 . embeddedResources
  where
    hasMatchingMD5 resource = md5 == resourceMD5 resource

-- | Insert a `Resource` into the embedded resources of a `Preset`.
insertResource :: Resource -> Preset -> Preset
insertResource resource@Resource{..}  preset@Preset{..} =
  preset { embeddedResources = Map.insert resourceName resource embeddedResources }

-- | Change a preset's metadata name.
--
-- This sets the name that is displayed inside Krita, not the
-- filename.
setPresetName :: Text -> Preset -> Preset
setPresetName name preset = preset { presetName = name }

-- | Get a preset's icon image as PNG data.
--
-- This icon will not contain any of the preset metadata and is just a
-- regular PNG file.
getPresetIcon :: Preset -> ByteString
getPresetIcon Preset{..} = runPut $ putMagicString *> traverse_ put (RegularChunk <$> presetIcon)

-- | Change a preset's icon image.
--
-- The new icon is passed in the form of PNG data. In the case the PNG
-- is a Krita preset, we strip out any existing preset metadata, since
-- we will be inserting our own later.
setPresetIcon :: MonadError String m => ByteString -> Preset -> m Preset
setPresetIcon pngData preset =
  case runGetOrFail (getMagicString *> some getChunk) pngData of
    Left  (_, _, err)    -> throwError err
    Right (_, _, chunks) -> pure preset { presetIcon = [c | RegularChunk c <- chunks] }

-- | Get the dimensions of the preset icon image.
presetIconDimensions :: Preset -> (Word32, Word32)
presetIconDimensions Preset{..} = runGet getIhdrDimensions $ head presetIcon

makeSettingXml :: Preset -> BL.ByteString
makeSettingXml preset =
  let documentPrologue = Prologue [] Nothing []
      documentEpilogue = []
      documentRoot     = renderXml_Preset preset
      renderSettings   = def { rsUseCDATA = const True }
  in renderLBS renderSettings Document{..}
