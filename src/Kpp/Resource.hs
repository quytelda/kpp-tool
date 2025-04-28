{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : Kpp.Resource
Copyright   : (c) Quytelda Kahja, 2024
License     : BSD-3-Clause

This module contains functions and data types for describing embedded
resources. An embedded resource is a Krita resource (e.g. brush tip,
pattern) which is embedded inside the XML of a preset file.
-}
module Kpp.Resource where
-- module Kpp.Resource
--   ( Resource(..)
--   , resourceMD5
--   , loadResource
--   , saveResource
--   , prettyResources
--   , parseXml_resource
--   , renderXml_resource
--   , parseXml_resources
--   , renderXml_resources
--   ) where

import           Control.Monad.Except
import qualified Crypto.Hash.MD5      as MD5
import qualified Data.ByteString      as BS
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import qualified Data.Text            as T
import           Prettyprinter        hiding (width)
import           Text.XML
import           System.FilePath

import           Kpp.Common

-- | 'Resource' is a type for embedded resources.
data Resource = Resource { resourceName :: !T.Text
                         , resourceFile :: !T.Text
                         , resourceType :: !T.Text
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

-- | Format a table of embedded resource entries.
prettyResources :: Map T.Text Resource -> Doc ann
prettyResources m | null m    = "None"
                  | otherwise = concatWith (<\\>) $ pretty <$> m

-- -- | Calculate the MD5 checksum of a `Resource` displayed in
-- -- hexadecimal notation.
-- resourceMD5 :: Resource -> T.Text
-- resourceMD5 = md5sum . resourceData

-- -- | Load a resource file.
-- loadResource :: FilePath -> T.Text -> Maybe T.Text -> Maybe T.Text -> IO Resource
-- loadResource path resourceType mname mfile = do
--   let resourceFile = fromMaybe (T.pack $ takeFileName path) mfile
--       resourceName = fromMaybe (T.pack $ takeFileName path) mname
--   resourceData <- BS.readFile path
--   return Resource{..}

-- -- | Save a resource, returning the path used.
-- saveResource :: Maybe FilePath -> Resource -> IO FilePath
-- saveResource mpath Resource{..} = do
--   let path = fromMaybe (T.unpack resourceFile) mpath
--   BS.writeFile path resourceData
--   return path

-- parseXml_resource :: MonadError String m => Element -> m Resource
-- parseXml_resource = withElement "resource" $ \e -> do
--   resourceName <- attributeText "name"     e
--   resourceType <- attributeText "type"     e
--   resourceFile <- attributeText "filename" e
--   resourceCsum <- attributeText "md5sum"   e >>= decodeBase16
--   resourceData <- contentText              e >>= decodeBase64

--   -- verify checksum
--   if resourceCsum == MD5.hash resourceData
--     then pure Resource{..}
--     else throwError $ "checksum mismatch for resource: " <> show resourceName

-- renderXml_resource :: Resource -> Element
-- renderXml_resource Resource{..} =
--   let resourceCsum      = md5sum resourceData
--       elementName       = "resource"
--       elementNodes      = [NodeContent $ encodeBase64 resourceData]
--       elementAttributes = Map.fromList [ ("name",     resourceName)
--                                        , ("filename", resourceFile)
--                                        , ("type",     resourceType)
--                                        , ("md5sum",   resourceCsum)
--                                        ]
--   in Element{..}

-- -- | Parse a @<resources>@ XML element, which should contain a list of
-- -- all embedded resources.
-- parseXml_resources :: MonadError String m => Element -> m (Map T.Text Resource)
-- parseXml_resources = withElement "resources" $ \e -> do
--   resources <- forM (childElements e) parseXml_resource
--   return $ Map.fromList $ zip (resourceName <$> resources) resources

-- renderXml_resources :: Map T.Text Resource -> Element
-- renderXml_resources rs =
--   let elementName       = "resources"
--       elementNodes      = NodeElement <$> renderXml_resource <$> Map.elems rs
--       elementAttributes = Map.empty
--   in Element{..}
