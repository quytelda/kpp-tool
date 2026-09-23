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
  ( -- * Error Handling
    ParseException(..)
  , eitherThrow

    -- * Encoding
  , encodeBase16
  , decodeBase16
  , encodeBase64
  , decodeBase64
  , decodeInt
  , md5sum

    -- * Pretty Printing
  , (<\>)
  , (<\\>)
  , prettyByteData

    -- * XML
  , makeDocument
  , elementToLBS
  , elementFromLBS
  , attributeText
  , contentText
  , childElements
  , withElement

    -- * PNG
  , pngMagicString
  , isPngData

    -- * Other
  , (.==)
  ) where

import           Conduit
import           Control.Exception
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

--------------------------------------------------------------------------------
-- Error Handling

-- | Exceptions that might be thrown while parsing preset files.
data ParseException = ParseException String
  deriving (Eq, Show)

instance Exception ParseException where
  displayException (ParseException e) = e

-- | Lift an @Either String@ into an instance of 'MonadThrow'.
eitherThrow :: MonadThrow m => Either String a -> m a
eitherThrow = either (throwM . ParseException) pure

-- | Helper function for succinctly comparing record fields.
--
-- >>> (resourceName .== "name") (Resource "name" "file" "type" "data")
-- True
(.==) :: Eq a => (t -> a) -> a -> t -> Bool
f .== x = \r -> f r == x

-- | Encode binary data into base-16 (hex) text.
encodeBase16 :: BS.ByteString -> T.Text
encodeBase16 = decodeUtf8 . Base16.encode

-- | Decode base-16 (hex) text into binary data.
decodeBase16 :: MonadThrow m => T.Text -> m BS.ByteString
decodeBase16 = eitherThrow . Base16.decode . encodeUtf8

-- | Encode binary data into base-64 text.
encodeBase64 :: BS.ByteString -> T.Text
encodeBase64 = decodeUtf8 . Base64.encode

-- | Decode base-64 text into binary data.
decodeBase64 :: MonadThrow m => T.Text -> m BS.ByteString
decodeBase64 = eitherThrow . Base64.decode . encodeUtf8

-- | Helper function to parse an Int from a Text value.
--
-- Note: This function fails if unconsumed data remains after parsing.
decodeInt :: MonadThrow m => T.Text -> m Int
decodeInt t = case Read.decimal t of
    Right (n, "") -> pure n
    _             -> throwM $ ParseException "input contains non-decimal digits"

-- | Calculate an MD5 checksum.
--
-- The result is returned in hexadecimal notation, and should match
-- the output of md5sum from the GNU coreutils.
md5sum :: BS.ByteString -> T.Text
md5sum = encodeBase16 . MD5.hash

--------------------------------------------------------------------------------
-- Pretty Printing

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

--------------------------------------------------------------------------------
-- XML

-- | Create a 'Document' with the given 'Element' as its root.
makeDocument :: Element -> Document
makeDocument documentRoot =
  let documentPrologue = Prologue [] Nothing []
      documentEpilogue = []
  in Document{..}

-- | Serialize an XML document given a root 'Element'.
elementToLBS :: Element -> BL.ByteString
elementToLBS = renderLBS renderSettings . makeDocument
  where
    renderSettings = def { rsUseCDATA = const True }

-- | Parse the root 'Element' from an XML document.
elementFromLBS :: MonadThrow m => BL.ByteString -> m Element
elementFromLBS xml =
  case parseLBS def xml of
    Right doc -> pure $ documentRoot doc
    Left  err -> throwM err

-- | Get the value of an 'Element' attribute or throw an error if it
-- doesn't exist.
attributeText :: MonadThrow m => Name -> Element -> m T.Text
attributeText name Element{..} =
  case Map.lookup name elementAttributes of
    Just v  -> pure v
    Nothing -> throwM $ ParseException $
      "missing attribute: " <> show (nameLocalName name)

-- | Get all the content nodes inside an element and combine them.
contentText :: Applicative f => Element -> f T.Text
contentText (Element _ _ nodes) = pure $ T.concat [text | NodeContent text <- nodes]

-- | Select the element children of a given `Element`.
childElements :: Element -> [Element]
childElements e = [child | NodeElement child <- elementNodes e]

-- | Check the `Element` name matches before doing some computation with it.
withElement :: MonadThrow m => Name -> (Element -> m a) -> Element -> m a
withElement name f e@Element{..}
  | name == elementName = f e
  | otherwise = throwM $ ParseException $ T.unpack $
    "expected \"" <> nameLocalName name        <> "\" element, " <>
    "found \""    <> nameLocalName elementName <> "\" element"

--------------------------------------------------------------------------------
-- PNG

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
