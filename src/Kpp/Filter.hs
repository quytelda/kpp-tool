{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : Kpp.Filter
Copyright   : (c) Quytelda Kahja, 2024
License     : BSD-3-Clause

This module contains functions and data structures for preset filter
configurations.
-}
module Kpp.Filter where
-- module Kpp.Filter
--   ( FilterConfig(..)
--   , prettyFilter
--   , parseXml_filterconfig
--   , renderXml_filterconfig
--   ) where

import           Conduit
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import           Prettyprinter          hiding (width)
import           Text.XML

import           Kpp.Common
import           Kpp.Param

-- | `FilterConfig` represents the serialized settings for a filter.
--
-- @<filterconfig>@ elements appears in filter preset settings and
-- contain a list of parameters. Acceptable versions seem to be "1"
-- and "2" as of Dec. 2024.
--
-- Note: Some parameters might be missing the type attribute.
data FilterConfig = FilterConfig
  { filterVersion :: !T.Text
  , filterParams  :: !(Map T.Text ParamValue)
  } deriving (Eq, Show)

instance Pretty FilterConfig where
  pretty FilterConfig{..} =
    parens ("version=" <> viaShow filterVersion)
    <\> prettyParams filterParams

-- | Format an optional table of filter settings.
prettyFilter :: Maybe FilterConfig -> Doc ann
prettyFilter Nothing             = "None"
prettyFilter (Just filterConfig) = pretty filterConfig

parseXml_filterconfig :: MonadThrow m => Element -> m FilterConfig
parseXml_filterconfig = withElement "filterconfig" $ \e -> do
  filterVersion <- attributeText "version" e
  filterParams  <- Map.fromList <$> traverse parseXml_param (childElements e)
  return FilterConfig{..}

renderXml_filterconfig :: FilterConfig -> Element
renderXml_filterconfig FilterConfig{..} =
  let elementName       = "filterconfig"
      elementNodes      = NodeElement <$> renderXml_params filterParams
      elementAttributes = Map.fromList [ ("version", filterVersion) ]
  in Element{..}
