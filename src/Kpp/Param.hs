{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : Kpp.Param
Copyright   : (c) Quytelda Kahja, 2024
License     : BSD-3-Clause

This module contains functions and data structures for parsing,
rendering, and manipulating brush presets (KPP files).
-}
module Kpp.Param
  ( ParamValue(..)
  , prettyParam
  , prettyParams
  , parseXml_param
  , renderXml_param
  , renderXml_params
  ) where

import           Control.Applicative
import           Control.Monad.Except
import qualified Data.ByteString        as BS
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import           Prettyprinter          hiding (width)
import           Text.XML

import           Kpp.Common

-- | `ParamValue` represents the value of a preset parameter.
--
-- Parameter values have an associated type which can be:
--
--   * "string" (for textual data)
--   * "internal" (no specific QVariant wrapper?)
--   * "bytearray" (for binary data encoded in base64)
--
-- However, sometimes the @type@ attribute is missing in older presets
-- or in some filter settings. For these cases, the type is unknown.
data ParamValue = Unknown  !T.Text
                | String   !T.Text
                | Internal !T.Text
                | Binary   !BS.ByteString
                deriving (Eq, Show)

instance Pretty ParamValue where
  pretty (String   val) = dquotes  (pretty val)
  pretty (Internal val) = squotes  (pretty val)
  pretty (Binary   val) = brackets (prettyByteData val)
  pretty (Unknown  val) = braces   (pretty val)

-- | Pretty printer for parameters.
--
-- Displays a simple "key: value" representation.
prettyParam :: T.Text -> ParamValue -> Doc ann
prettyParam key val = pretty key <> ":" <+> pretty val

-- | Format a table of parameter names and values.
prettyParams :: Map T.Text ParamValue -> Doc ann
prettyParams = concatWith (<\>) . Map.mapWithKey prettyParam

parseXml_param :: MonadError String m => Element -> m (T.Text, ParamValue)
parseXml_param = withElement "param" $ \e@Element{..} -> do
  paramName <- attributeText "name" e
  paramData <- contentText e

  paramValue <- case Map.lookup "type" elementAttributes of
    Nothing          -> Unknown  <$> pure         paramData
    Just "string"    -> String   <$> pure         paramData
    Just "internal"  -> Internal <$> pure         paramData
    Just "bytearray" -> Binary   <$> decodeBase64 paramData
    Just paramType   -> throwError $ "unrecognized param type: " <> show paramType
  return (paramName, paramValue)

renderXml_param :: T.Text -> ParamValue -> Element
renderXml_param key val =
  let (paramType, paramData) = case val of
        Unknown  v -> (Nothing,          v)
        String   v -> (Just "string",    v)
        Internal v -> (Just "internal",  v)
        Binary   v -> (Just "bytearray", encodeBase64 v)
      elementName       = "param"
      elementNodes      = [NodeContent paramData]
      elementAttributes = Map.fromList $ [("name", key)]
                          <> maybe empty (\t -> [("type", t)]) paramType
  in Element{..}

renderXml_params :: Map T.Text ParamValue -> [Element]
renderXml_params = Map.elems . Map.mapWithKey renderXml_param
