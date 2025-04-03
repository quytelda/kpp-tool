{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Kpp.ParamSpec (spec) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Either
import qualified Data.Text            as T
import           Test.Hspec

import           Common
import           Kpp.Param

testParse :: T.Text -> ParamValue -> BL.ByteString -> Expectation
testParse name val xml = parseXml_param (parseXmlElement xml) `shouldBe` Right (name, val)

testParseFailure :: BL.ByteString -> Expectation
testParseFailure xml = parseXml_param (parseXmlElement xml) `shouldSatisfy` isLeft

testRender :: T.Text -> ParamValue -> Expectation
testRender name val = parseXml_param (renderXml_param name val) `shouldBe` Right (name, val)

spec :: Spec
spec = do
  describe "parseXml_param" $ do
    it "parses untyped parameters" $ do
      testParse "ParamName" (Unknown "ParamValue")
        "<param name=\"ParamName\"><![CDATA[ParamValue]]></param>"

    it "parses string parameters" $ do
      testParse "ParamName" (String "ParamValue")
        "<param type=\"string\" name=\"ParamName\"><![CDATA[ParamValue]]></param>"

    it "parses internal parameters" $ do
      testParse "ParamName" (Internal "ParamValue")
        "<param type=\"internal\" name=\"ParamName\"><![CDATA[ParamValue]]></param>"

    it "parses binary parameters" $ do
      testParse "ParamName" (Binary $ BS.pack [1..16])
        "<param type=\"bytearray\" name=\"ParamName\"><![CDATA[AQIDBAUGBwgJCgsMDQ4PEA==]]></param>"

    it "fails for invalid parameter types" $ do
      testParseFailure "<param type=\"wrong\" name=\"ParamName\"><![CDATA[ParamValue]]></param>"

    it "requires a parameter name" $ do
      testParseFailure "<param type=\"string\"><![CDATA[ParamValue]]></param>"

  describe "renderXml_param" $ do
    it "renders string presets" $ do
      testRender "ParamName" (String "ParamValue")

    it "renders internal parameters" $ do
      testRender "ParamName" (Internal "ParamValue")

    it "renders binary parameters" $ do
      testRender "ParamName" (Binary $ BS.pack [1..16])

    it "renders parameters with unknown type" $ do
      testRender "ParamName" (Unknown "ParamValue")

