{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : Kpp.Common
Copyright   : (c) Quytelda Kahja, 2025
License     : BSD-3-Clause

Common functionality and utility functions for multiple modules.
-}
module Kpp.Common
  ( encodeBase16
  , decodeBase16
  , encodeBase64
  , decodeBase64
  , decodeInt
  , md5sum
  , (<\>)
  , (<\\>)
  , prettyByteData
  , elementToLBS
  , elementFromLBS
  , attributeText
  , contentText
  , childElements
  , withElement
  , pngMagicString
  , isPngData
  ) where

import           Control.Exception
import           Control.Monad.Except
import qualified Crypto.Hash.MD5        as MD5
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import           Data.Text.Encoding
import qualified Data.Text.Read         as Read
import           Prettyprinter          hiding (width)
import           Text.XML

-- | Encode binary data into a base-16 (hex) string.
encodeBase16 :: BS.ByteString -> T.Text
encodeBase16 = decodeUtf8 . Base16.encode

-- | Decode a base-16 (hex) string into binary data.
decodeBase16 :: MonadError String m => T.Text -> m BS.ByteString
decodeBase16 = liftEither . Base16.decode . encodeUtf8

-- | Encode binary data into a base-64 string.
encodeBase64 :: BS.ByteString -> T.Text
encodeBase64 = decodeUtf8 . Base64.encode

-- | Decode a base-64 string into binary data.
--
-- Krita presets sometimes contain binary data which is base64-encoded
-- twice. I don't know if that is intentional or a bug since there is
-- no obvious pattern to which data is double encoded. Therefore, we
-- try base64-decoding all encoded data twice if possible.
decodeBase64 :: MonadError String m => T.Text -> m BS.ByteString
decodeBase64 t =
  let bs1 = Base64.decode $ encodeUtf8 t
      bs2 = Base64.decode =<< bs1
  in liftEither $ bs2 <> bs1

-- | Helper function to parse an Int from a Text value.
--
-- Note: This function fails if unconsumed data remains after parsing.
decodeInt :: MonadError String m => T.Text -> m Int
decodeInt t = case Read.decimal t of
    Right (n, "") -> pure n
    _             -> throwError "input contains non-decimal digits"

-- | Calculate an MD5 checksum.
--
-- The result is returned in hexadecimal notation, and should match
-- the output of md5sum from the GNU coreutils.
md5sum :: BS.ByteString -> T.Text
md5sum = encodeBase16 . MD5.hash

---------------------
-- Pretty Printing --
---------------------

-- | Separate documents using a line break.
(<\>) :: Doc ann -> Doc ann -> Doc ann
x <\> y = x <> line <> y

-- | Seperate documents using two line breaks.
(<\\>) :: Doc ann -> Doc ann -> Doc ann
x <\\> y = x <> line <> line <> y

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

---------
-- XML --
---------

elementToLBS :: Element -> BL.ByteString
elementToLBS documentRoot =
  let documentPrologue = Prologue [] Nothing []
      documentEpilogue = []
      renderSettings   = def { rsUseCDATA = const True }
  in renderLBS renderSettings Document{..}

elementFromLBS :: MonadError String m => BL.ByteString -> m Element
elementFromLBS xml =
  case parseLBS def xml of
    Right doc -> pure $ documentRoot doc
    Left  err -> throwError $ displayException err

attributeText :: MonadError String m => Name -> Element -> m T.Text
attributeText name Element{..} =
  case Map.lookup name elementAttributes of
    Just v  -> pure v
    Nothing -> throwError $ "missing attribute: " <> show (nameLocalName name)

-- | Get all the content nodes inside an element and combine them.
contentText :: Applicative f => Element -> f T.Text
contentText (Element _ _ nodes) = pure $ T.concat [text | NodeContent text <- nodes]

-- | Select the element children of a given `Element`.
childElements :: Element -> [Element]
childElements e = [child | NodeElement child <- elementNodes e]

-- | Check the `Element` name matches before doing some computation with it.
withElement :: MonadError String m => Name -> (Element -> m a) -> Element -> m a
withElement name f e@Element{..}
  | name == elementName = f e
  | otherwise = throwError $ T.unpack $
    "expected \"" <> nameLocalName name        <> "\" element, " <>
    "found \""    <> nameLocalName elementName <> "\" element"

---------
-- PNG --
---------

-- | All PNG image files begin with this signature.
pngMagicString :: BL.ByteString
pngMagicString = "\x89\x50\x4E\x47\x0D\x0A\x1A\x0A"

-- | Check whether a `BS.ByteString` represents a PNG image.
--
-- This function doesn't attempt to fully parse or validate the input.
-- It simply checks whether the input begins with the standard PNG
-- magic string.
isPngData :: BS.ByteString -> Bool
isPngData bs = BS.toStrict pngMagicString `BS.isPrefixOf` bs
