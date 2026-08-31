{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
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
  ( Preset(..)
  , lookupParam
  , insertParam
  , lookupResourceByName
  , lookupResourceByFile
  , lookupResourceByMD5
  , insertResource
  , setPresetName
  , setPresetIcon
  , presetIconDimensions

    -- * Conduits
  , pngToPreset
  , presetToPng

    -- * I/O
  , loadPreset
  , savePreset

    -- * XML
  , parseXml_Preset
  , renderXml_Preset
  ) where

import           Conduit
import           Control.Applicative
import           Control.Monad
import           Data.Binary
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Base64   as Base64
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Conduit.Combinators as C
import           Data.Foldable
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Maybe
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Prettyprinter            hiding (width)
import           Text.XML

import           Kpp.Common
import           Kpp.Filter
import           Kpp.Param
import           Kpp.Png
import           Kpp.Resource

-- | A `Preset` represents a Krita brush preset, including its
-- settings and any embedded resources.
data Preset = Preset
  { presetVersion     :: !BS.ByteString
  , presetName        :: !Text
  , presetPaintop     :: !Text
  , presetParams      :: !(Map Text ParamValue)
  , presetFilter      :: !(Maybe FilterConfig)
  , embeddedResources :: !(Map Text Resource)
  , presetIcon        :: !BL.ByteString
  } deriving (Eq, Show)

instance Pretty Preset where
  pretty preset@Preset{..} =
    vsep [ "name:"    <+> pretty  presetName
         , "version:" <+> viaShow presetVersion
         , "paintop:" <+> pretty  presetPaintop
         , "icon:"    <+> prettyIconDimensions
         ]
    <\\> nest 2 ("Parameters:"          <\> prettyParams    presetParams)
    <\\> nest 2 ("Filter Settings:"     <\> prettyFilter    presetFilter)
    <\\> nest 2 ("Embedded Resources:"  <\> prettyResources embeddedResources)
    where
      prettyIconDimensions =
        case presetIconDimensions preset of
          Just (width, height) -> pretty width <> "x" <> pretty height
          Nothing              -> "invalid PNG data"

--------------------------------------------------------------------------------
-- XML

-- | Parse a @<Preset>@ XML element, which should be the root element
-- of the preset settings document.
parseXml_Preset :: MonadThrow m => BS.ByteString -> BL.ByteString -> Element -> m Preset
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
              else throwM $ ParseException "found multiple <filterconfig> elements"
          "resources"    -> do
            resourceMap <- parseXml_resources child
            pure (params, filters, resourceMap <> resources)
          name           -> throwM $ ParseException $
            "unrecognized element: " <> T.unpack (nameLocalName name)
    ) (mempty, empty, mempty) (childElements e)

  -- If an expected resource count is provided we check it for
  -- accuracy; otherwise we can only assume all resources are present.
  case Map.lookup "embedded_resources" (elementAttributes e) of
    Just val -> do
      resourceCount <- decodeInt val
      unless (resourceCount == Map.size embeddedResources) $
        throwM $ ParseException "resource count mismatch"
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

--------------------------------------------------------------------------------
-- Conduits

sinkPreset
  :: MonadThrow m
  => ConduitT ParsedChunk Void m Preset
sinkPreset = do
  (version, doc, icon) <- getZipSink $ (,,)
    <$> ZipSink (selectVersion  .| sinkLazy)
    <*> ZipSink (selectSettings .| sinkDoc def)
    <*> ZipSink (selectIgnored  .| wrapChunks .| sinkLazy)

  parseXml_Preset (BS.toStrict version) icon (documentRoot doc)
    >>= doubleDecodePatterns

-- | Decode a binary stream into a 'Preset'.
pngToPreset :: MonadThrow m => ConduitT ByteString Void m Preset
pngToPreset = unwrapChunks .| C.map parseChunk .| sinkPreset

-- | Encode a 'Preset' as a stream of 'ByteString's.
presetToPng :: MonadThrow m => Preset -> ConduitT i ByteString m ()
presetToPng preset@Preset{..} =
  sourceLazy presetIcon
  .| unwrapChunks
  .| (do C.take 2
         yield
           $ renderVersionChunk presetVersion
         yield
           $ renderSettingChunk
           $ renderLBS def
           $ makeDocument
           $ renderXml_Preset
           $ doubleEncodePatterns preset
         awaitForever yield
     )
  .| wrapChunks

-- | Read and parse a KPP file.
loadPreset :: FilePath -> IO Preset
loadPreset path = runConduitRes $ sourceFile path .| pngToPreset

-- | Render and write a KPP file.
savePreset :: FilePath -> Preset -> IO ()
savePreset path preset = runConduitRes $ presetToPng preset .| sinkFile path

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

-- | Change a preset's icon image.
--
-- The new icon is passed in the form of PNG data. If case the
-- provided PNG is a Krita preset, we strip out any existing preset
-- metadata, since we will be inserting our own later.
setPresetIcon :: MonadThrow m => BL.ByteString -> Preset -> m Preset
setPresetIcon pngData preset = do
  icon <- runConduit
    $ sourceLazy pngData
    .| unwrapChunks
    .| C.filter (not . isSpecialChunk)
    .| wrapChunks
    .| sinkLazy

  return preset { presetIcon = icon }

-- | Get the dimensions of the preset icon image.
presetIconDimensions :: MonadThrow m => Preset -> m (Word32, Word32)
presetIconDimensions Preset{..} = runConduit $ sourceLazy presetIcon .| pngDimensions

--------------------------------------------------------------------------------
-- Fixes

-- For presets with version 2.2, any embedded pattern is saved in the
-- "Texture/Pattern/Pattern" parameter as a binary value. In this
-- case, there will also be a matching "Texture/Pattern/PatternMD5"
-- parameter which contains an MD5 checksum. However, both these
-- values will be base64-encoded twice.
--
-- To get the actual data, we must decode these values a second time.
-- Similarly when we saving the preset, we have to encode the data
-- twice before inserting it into the settings document.
--
-- TODO: Does Krita actually require the values to be double-encoded
-- to load them correctly? If not, we could just output single-encoded
-- values. Maybe test this against old and new Krita versions.
--
-- TODO: Create some presets using pre-5.0 Krita versions for testing.

keyPattern :: T.Text
keyPattern    = "Texture/Pattern/Pattern"

keyPatternMD5 :: T.Text
keyPatternMD5 = "Texture/Pattern/PatternMD5"

doubleDecodePatterns :: MonadThrow m => Preset -> m Preset
doubleDecodePatterns preset@Preset{..}
  | presetVersion == "5.0" = pure preset
  | otherwise = do
      params <- mapAdjustA decodeParam keyPattern >=>
                mapAdjustA decodeParam keyPatternMD5 $ presetParams
      return preset { presetParams = params }
  where
    decodeParam (Binary bs) = Binary <$> (eitherThrow . Base64.decode) bs
    decodeParam x           = pure x
    mapAdjustA = Map.alterF . traverse

doubleEncodePatterns :: Preset -> Preset
doubleEncodePatterns preset@Preset{..}
  | presetVersion == "5.0" = preset
  | otherwise =
      let params = Map.adjust encodeParam keyPattern .
                   Map.adjust encodeParam keyPatternMD5 $ presetParams
      in preset { presetParams = params }
  where
    encodeParam (Binary bs) = Binary $ Base64.encode bs
    encodeParam x           = x
