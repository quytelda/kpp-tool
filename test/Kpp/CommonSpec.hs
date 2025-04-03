{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Kpp.CommonSpec (spec) where

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BL
import           Data.Char
import qualified Data.Text                 as T
import           Prettyprinter
import           Prettyprinter.Render.Text
import           Test.Hspec
import           Text.XML

import           Common
import           Kpp.Common

exampleXml :: BL.ByteString
exampleXml = "<example key=\"value\">content</example>"

spec :: Spec
spec = do
  let binData = BS.pack [0x37, 0x5f, 0x00]
  describe "encodeBase16" $ do
    it "encodes base-16 data" $ do
      encodeBase16 binData `shouldBe` "375f00"

  describe "decodeBase16" $ do
    it "decodes base-16 data" $ do
      decodeBase16 "375f00" `shouldBe` Right binData
      decodeBase16 "375F00" `shouldBe` Right binData

  describe "encodeBase64" $ do
    it "encodes base-64 data" $ do
      encodeBase64 binData `shouldBe` "N18A"

  describe "decodeBase64" $ do
    it "decodes base-64 data" $ do
      decodeBase64 "N18A" `shouldBe` Right binData

    it "decodes double-encoded base-64 data" $ do
      decodeBase64 "TjE4QQ==" `shouldBe` Right binData

  describe "decodeInt" $ do
    it "decodes a positive integer" $ do
      decodeInt "13" `shouldBe` Right 13

    it "fails when provided non-decimal digits" $ do
      decodeInt "13p" `shouldBe` Left "input contains non-decimal digits"

    it "fails when input contains spaces" $ do
      decodeInt " 13 " `shouldBe` Left "input contains non-decimal digits"

  describe "md5sum" $ do
    it "computes an MD5 checksum" $ do
      md5sum "example" `shouldBe` "1a79a4d60de6718e8e5b326e338ae533"

    it "uses lowercase hexadecimal notation" $ do
      let isHex = T.all (\c -> isDigit c || (c >= 'a' && c <= 'f'))
      md5sum "example" `shouldSatisfy` isHex

  describe "prettyByteData" $ do
    it "renders short strings directly" $ do
      let result = renderStrict $ layoutCompact $ prettyByteData binData
      result `shouldBe` "\"7_\\NUL\""

    it "renders a summary for long strings" $ do
      let result = renderStrict $ layoutCompact $ prettyByteData $ BS.pack [1..64]
      result `shouldBe` "Binary Data (64 bytes)"

    it "renders a summary for PNG data" $ do
      png <- BS.readFile path_1px
      let result = renderStrict $ layoutCompact $ prettyByteData png
      result `shouldBe` "PNG Image (546 bytes)"

  describe "attributeText" $ do
    let root = parseXmlElement exampleXml

    it "returns an XML attribute's value" $ do
      attributeText "key" root `shouldBe` Right "value"

    it "fails when requested attribute is missing" $ do
      attributeText "nonexistant" root `shouldBe` Left "missing attribute: \"nonexistant\""

  describe "contentText" $ do
    it "returns the content of an XML element" $ do
      let root = parseXmlElement exampleXml
      contentText root `shouldBe` Just "content"

    it "returns empty string when content is missing" $ do
      let root = parseXmlElement "<example key=\"value\" />"
      contentText root `shouldBe` Just ""

  describe "isPngData" $ do
    it "recognizes PNG data" $ do
      png <- BS.readFile path_1px
      png `shouldSatisfy` isPngData

    it "recognizes invalid PNG data" $ do
      let binData' = "\x89\x50\x4E\x47\x0D\x0E\x1A\x0A"
      binData' `shouldSatisfy` (not . isPngData)
