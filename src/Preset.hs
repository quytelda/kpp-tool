{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Preset
  ( Preset(..)
  , presetIconDimensions
  , prettyParams
  , prettyResources
  , ParamValue(..)
  , Resource(..)
  , resourceMD5
  , lookupParam
  , insertParam
  , lookupResourceByName
  , lookupResourceByFile
  , lookupResourceByMD5
  , insertResource
  , setPresetName
  ) where

import           Codec.Compression.Zlib
import           Control.Applicative
import           Control.Monad
import qualified Crypto.Hash.MD5        as MD5
import           Data.Bifunctor         (first)
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.ByteString.Lazy   as BL
import           Data.Digest.CRC32
import           Data.Foldable
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding
import qualified Data.Text.Read         as Read
import           Prettyprinter          hiding (width)
import           Text.XML

-----------------------------
-- PNG Parsing & Rendering --
-----------------------------

getNull :: Get ()
getNull = do
  b <- getWord8
  guard $ b == 0

putNull :: Put
putNull = putWord8 0

expect :: ByteString -> Get ()
expect expected = do
  actual <- getLazyByteString $ BL.length expected
  guard $ actual == expected

pngMagicString :: ByteString
pngMagicString = "\x89\x50\x4E\x47\x0D\x0A\x1A\x0A"

getMagicString :: Get ()
getMagicString = expect pngMagicString

putMagicString :: Put
putMagicString = putLazyByteString pngMagicString

getChunk :: Get ByteString
getChunk = do
  chunkLength <- getWord32be
  chunkData   <- getLazyByteString $ fromIntegral (4 + chunkLength)
  chunkCsum   <- getWord32be

  if chunkCsum == crc32 chunkData
    then return chunkData
    else fail "checksum mismatch"

putChunk :: ByteString -> Put
putChunk chunkData = do
  putWord32be chunkLength
  putLazyByteString chunkData
  putWord32be chunkCsum
  where
    chunkLength = fromIntegral $ BL.length chunkData - 4
    chunkCsum   = crc32 chunkData

getTextChunk :: ByteString -> Get ByteString
getTextChunk key = do
  expect "tEXt"
  expect key *> getNull
  getRemainingLazyByteString

putTextChunk :: ByteString -> ByteString -> Put
putTextChunk key value = do
  putLazyByteString "tEXt"
  putLazyByteString key *> putNull
  putLazyByteString value

getZtxtChunk :: ByteString -> Get ByteString
getZtxtChunk keyword = do
  expect "zTXt"
  expect keyword *> getNull
  void getWord8 -- compression type is always 0
  decompress <$> getRemainingLazyByteString

putZtxtChunk :: ByteString -> ByteString -> Put
putZtxtChunk key value = do
  putLazyByteString "zTXt"
  putLazyByteString key *> putNull
  putWord8 0 -- compression type is always 0
  putLazyByteString $ compress value

getItxtChunk :: ByteString -> Get ByteString
getItxtChunk keyword = do
  expect "iTXt"
  expect keyword *> getNull
  compressed <- get :: Get Bool
  void getWord8 -- compression type is always 0
  void getLazyByteStringNul -- ignore language tag
  void getLazyByteStringNul -- ignore translated keyword
  content <- getRemainingLazyByteString

  return $ if compressed
           then decompress content
           else content

getIhdrDimensions :: Get (Word32, Word32)
getIhdrDimensions = do
  expect "IHDR"
  width  <- getWord32be
  height <- getWord32be
  return (width, height)

getKeywordChunk :: ByteString -> Get ByteString
getKeywordChunk key = getTextChunk key <|>
                      getZtxtChunk key <|>
                      getItxtChunk key

getVersionChunk :: Get BS.ByteString
getVersionChunk = BS.toStrict <$> getKeywordChunk "version"

putVersionChunk :: BS.ByteString -> Put
putVersionChunk = putTextChunk "version" . BS.fromStrict

getSettingChunk :: Get ByteString
getSettingChunk = getKeywordChunk "preset"

putSettingChunk :: Preset -> Put
putSettingChunk preset =
  let documentPrologue = Prologue [] Nothing []
      documentEpilogue = []
      documentRoot     = renderXmlPreset preset
      renderSettings   = def { rsUseCDATA = const True }
      xml              = renderLBS renderSettings Document{..}
  in putZtxtChunk "preset" xml

-------------------------------------------
-- Preset (KPP file) Parsing & Rendering --
-------------------------------------------

-- | Separate documents using a line break.
(<\>) :: Doc ann -> Doc ann -> Doc ann
x <\> y = x <> line <> y

-- | Seperate documents using two line breaks.
(<\\>) :: Doc ann -> Doc ann -> Doc ann
x <\\> y = x <> line <> line <> y

-- | A `Preset` represents a Krita brush preset, including it's
-- settings and any embedded resources.
data Preset = Preset
  { presetVersion     :: !BS.ByteString
  , presetName        :: !Text
  , presetPaintop     :: !Text
  , presetParams      :: !(Map Text ParamValue)
  , embeddedResources :: !(Map Text Resource)
  , presetIcon        :: ![ByteString]
  } deriving (Show)

-- | Get the dimensions of the preset icon image.
presetIconDimensions :: Preset -> (Word32, Word32)
presetIconDimensions Preset{..} = runGet getIhdrDimensions $ head presetIcon

-- | Format a parameter table.
prettyParams :: Map Text ParamValue -> Doc ann
prettyParams = concatWith (<\>) . Map.mapWithKey prettyParam

-- | Format a resource table.
prettyResources :: Map Text Resource -> Doc ann
prettyResources = concatWith (<\\>) . fmap pretty

instance Pretty Preset where
  pretty preset@Preset{..} =
    vsep [ "name:"    <+> pretty  presetName
         , "version:" <+> viaShow presetVersion
         , "paintop:" <+> pretty  presetPaintop
         , "icon:"    <+> pretty width <> "x" <> pretty height
         ]
    <\\> nest 2 ("Parameters:" <\> prettyParams    presetParams)
    <\\> nest 2 ("Resources:"  <\> prettyResources embeddedResources)
    where
      (width, height) = presetIconDimensions preset

instance Binary Preset where
  get = getPreset
  put = putPreset

getPreset :: Get Preset
getPreset = do
  getMagicString
  chunks <- many getChunk

  case foldr addChunk ([],[],[]) chunks of
    ([version], [settings], chunks') -> either fail pure
                                        $ parseXmlPreset version chunks'
                                        $ documentRoot
                                        $ parseLBS_ def settings
    ([]       , _         , _      ) -> fail "missing preset version chunk"
    (_        , []        , _      ) -> fail "missing preset settings chunk"
    _                                -> fail "duplicated metadata chunks"
  where
    slot1 a (xs, ys, zs) = (a:xs,   ys,   zs)
    slot2 a (xs, ys, zs) = (  xs, a:ys,   zs)
    slot3 a (xs, ys, zs) = (  xs,   ys, a:zs)
    addChunk chunk = flip runGet chunk $
      slot1 <$> getVersionChunk <|>
      slot2 <$> getSettingChunk <|>
      slot3 <$> pure chunk

putPreset :: Preset -> Put
putPreset preset@Preset{..} = do
  -- The metadata chunks must be inserted after the IHDR chunk.
  -- Krita inserts the elements following the IHDR and pHYs chunks,
  -- but before the IDAT chunks, so this code matches the behavior.
  let isFollower bs = BL.isPrefixOf "IDAT" bs || BL.isPrefixOf "IEND" bs
      (pre, post) = break isFollower presetIcon
      versionChunk = runPut $ putVersionChunk presetVersion
      settingChunk = runPut $ putSettingChunk preset

  putMagicString
  traverse_ putChunk pre
  putChunk settingChunk
  putChunk versionChunk
  traverse_ putChunk post

-----------------------------
-- XML Parsing & Rendering --
-----------------------------

encodeBase16 :: BS.ByteString -> Text
encodeBase16 = decodeUtf8 . Base16.encode

decodeBase16 :: Text -> Either String BS.ByteString
decodeBase16 = Base16.decode . encodeUtf8

encodeBase64 :: BS.ByteString -> Text
encodeBase64 = decodeUtf8 . Base64.encode

decodeBase64 :: Text -> Either String BS.ByteString
decodeBase64 t =
  -- Krita presets sometimes contain binary data which is
  -- base64-encoded twice. I don't know if that is intentional or a
  -- bug since there is no obvious pattern to which data is double
  -- encoded. Therefore, we try base64-decoding all encoded data twice
  -- if possible.
  let bs1 = Base64.decode $ encodeUtf8 t
      bs2 = Base64.decode =<< bs1
  in bs2 <> bs1

md5sum :: BS.ByteString -> Text
md5sum = encodeBase16 . MD5.hash

-- | Helper function to parse an Int from a Text value.
--
-- Note: This function fails if unconsumed data remains after parsing.
decodeInt :: Text -> Either String Int
decodeInt t = case Read.decimal t of
    Right (n, "") -> Right n
    _             -> Left "input contains non-decimal digits"

attributeText :: Name -> Element -> Either String Text
attributeText name Element{..} =
  case Map.lookup name elementAttributes of
    Just v  -> Right v
    Nothing -> Left $ "missing attribute: " <> (show $ nameLocalName name)

-- | Get all the content nodes inside an element and combine them.
contentText :: Element -> Either String Text
contentText (Element _ _ nodes) =
  case [text | NodeContent text <- nodes] of
    [] -> Left "element has no text content"
    ts -> Right $ T.concat ts

-- | Select the element children of a given `Element`.
childElements :: Element -> [Element]
childElements e = [child | NodeElement child <- elementNodes e]

isPngData :: BS.ByteString -> Bool
isPngData bs = BS.toStrict pngMagicString `BS.isPrefixOf` bs

-- | Pretty printer for arbitrary blobs of binary data.
--
-- Small blobs are displayed normally (using show).
-- If the blob contains a PNG image, display "[PNG Image (<size> bytes)]"
-- Otherwise, display "[Binary Data (<size> bytes)]"
prettyByteData :: BS.ByteString -> Doc ann
prettyByteData bytes
  | size <= maxlen = viaShow bytes
  | otherwise      = description <+> parens (pretty size <+> "bytes")
  where
    maxlen      = 32
    size        = BS.length bytes
    description = if isPngData bytes
                  then "PNG Image"
                  else "Binary Data"

prettyParam :: Text -> ParamValue -> Doc ann
prettyParam key val = pretty key <> ":" <+> pretty val

-- | `ParamValue` represents the value of a preset parameter.
--
-- Parameter values have an associated type which can be:
--   * "string" (for textual data)
--   * "internal" (no specific QVariant wrapper)
--   * "bytearray" (for binary data encoded in base64)
data ParamValue = String   !Text
                | Internal !Text
                | Binary   !BS.ByteString
                deriving (Show)

instance Pretty ParamValue where
  pretty (String   val) = dquotes  (pretty val)
  pretty (Internal val) = squotes  (pretty val)
  pretty (Binary   val) = brackets (prettyByteData val)

parseXmlParam :: Element -> Either String (Text, ParamValue)
parseXmlParam e@(Element "param" _ _) = do
  paramName <- attributeText "name" e
  paramType <- attributeText "type" e
  paramData <- contentText e

  paramValue <- case paramType of
    "string"    -> String   <$> pure         paramData
    "internal"  -> Internal <$> pure         paramData
    "bytearray" -> Binary   <$> decodeBase64 paramData
    _           -> Left $ "unknown param type: " <> show paramType
  return (paramName, paramValue)
parseXmlParam _ = Left "expected <param> element"

renderXmlParam :: Text -> ParamValue -> Element
renderXmlParam key val =
  let (paramType, paramData) = case val of
        String   v -> ("string",    v)
        Internal v -> ("internal",  v)
        Binary   v -> ("bytearray", encodeBase64 v)
      elementName       = "param"
      elementNodes      = [NodeContent paramData]
      elementAttributes = Map.fromList [ ("name", key)
                                       , ("type", paramType)
                                       ]
  in Element{..}

renderXmlParams :: Map Text ParamValue -> [Element]
renderXmlParams = Map.elems . Map.mapWithKey renderXmlParam

-- | 'Resource' is a type for embedded resources.
data Resource = Resource { resourceName :: !Text
                         , resourceFile :: !Text
                         , resourceType :: !Text
                         , resourceData :: !BS.ByteString
                         } deriving (Show)

instance Pretty Resource where
  pretty Resource{..} =
    vsep [ "name:" <+> pretty resourceName
         , "file:" <+> pretty resourceFile
         , "type:" <+> pretty resourceType
         , "data:" <+> prettyByteData resourceData
         , "md5:"  <+> pretty (md5sum resourceData)
         ]

parseXmlResource :: Element -> Either String Resource
parseXmlResource e@(Element "resource" _ _) = do
  resourceName <- attributeText "name"     e
  resourceType <- attributeText "type"     e
  resourceFile <- attributeText "filename" e
  resourceCsum <- attributeText "md5sum"   e >>= decodeBase16
  resourceData <- contentText              e >>= decodeBase64

  -- verify checksum
  if resourceCsum == MD5.hash resourceData
    then Right Resource{..}
    else Left $ "checksum mismatch for resource: " <> show resourceName
parseXmlResource _ = Left "expected <resource> element"

renderXmlResource :: Resource -> Element
renderXmlResource Resource{..} =
  let resourceCsum      = md5sum resourceData
      elementName       = "resource"
      elementNodes      = [NodeContent $ encodeBase64 resourceData]
      elementAttributes = Map.fromList [ ("name",     resourceName)
                                       , ("filename", resourceFile)
                                       , ("type",     resourceType)
                                       , ("md5sum",   resourceCsum)
                                       ]
  in Element{..}

parseXmlResources :: Element -> Either String (Map Text Resource)
parseXmlResources e@(Element "resources" _ _) = do
  resources <- forM (childElements e) parseXmlResource
  return $ Map.fromList $ zip (resourceName <$> resources) resources
parseXmlResources _ = Left "expected <resources> element"

renderXmlResources :: Map Text Resource -> Element
renderXmlResources rs =
  let elementName       = "resources"
      elementNodes      = NodeElement <$> renderXmlResource <$> Map.elems rs
      elementAttributes = Map.empty
  in Element{..}

-- | Parse a @<Preset>@ XML element, which should be the root element
-- of the preset settings document.
parseXmlPreset :: BS.ByteString -> [ByteString] -> Element -> Either String Preset
parseXmlPreset presetVersion presetIcon e@(Element "Preset" _ _) = do
  presetName    <- attributeText "name"      e
  presetPaintop <- attributeText "paintopid" e
  resourceCount <- attributeText "embedded_resources" e >>= decodeInt

  (presetParams, embeddedResources) <- first Map.fromList <$>
    foldM addChild (mempty, mempty) (childElements e)

  if resourceCount == Map.size embeddedResources
    then Right Preset{..}
    else Left "resource count mismatch"
  where
    addChild (xs, ys) child =
      ((\x -> (x:xs,      ys)) <$> parseXmlParam     child) <>
      ((\y -> (  xs, y <> ys)) <$> parseXmlResources child)
parseXmlPreset _ _ _ = Left "expected <Preset> element"

renderXmlPreset :: Preset -> Element
renderXmlPreset Preset{..} =
  let paramNodes        = renderXmlParams    presetParams
      resourcesNode     = renderXmlResources embeddedResources
      resourceCount     = T.pack $ show $ length embeddedResources
      elementName       = "Preset"
      elementNodes      = NodeElement <$> resourcesNode : paramNodes
      elementAttributes = Map.fromList [ ("name",      presetName)
                                       , ("paintopid", presetPaintop)
                                       , ("embedded_resources", resourceCount)
                                       ]
  in Element{..}

---------------------------
-- Convenience Functions --
---------------------------

-- | Calculate the MD5 checksum of a `Resource` displayed in
-- hexadecimal notation.
resourceMD5 :: Resource -> Text
resourceMD5 = md5sum . resourceData

-- | Look up the value of a preset parameter.
lookupParam :: Text -> Preset -> Maybe ParamValue
lookupParam key = Map.lookup key . presetParams

-- | Insert or update a preset parameter.
insertParam :: Text -> ParamValue -> Preset -> Preset
insertParam key val preset@Preset{..} =
  preset { presetParams = Map.insert key val presetParams }

-- | Look up a resource by name.
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
