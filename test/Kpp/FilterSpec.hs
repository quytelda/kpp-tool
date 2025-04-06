{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Kpp.FilterSpec (spec) where

import qualified Data.ByteString.Lazy as BL
import           Data.Either
import qualified Data.Map.Strict      as Map
import           Test.Hspec

import           Common
import           Kpp.Filter
import           Kpp.Param

exampleXml :: BL.ByteString
exampleXml = "<filterconfig version=\"2\"><param name=\"example\">value</param></filterconfig>"

parses :: (FilterConfig -> a) -> BL.ByteString -> Either String a
parses f = fmap f . parseXml_filterconfig . parseXmlElement

testParse :: FilterConfig -> BL.ByteString -> Expectation
testParse expected xml = parses id xml `shouldBe` Right expected

testParseFailure :: BL.ByteString -> Expectation
testParseFailure xml = parses id xml `shouldSatisfy` isLeft

testRender :: FilterConfig -> Expectation
testRender fc = parseXml_filterconfig (renderXml_filterconfig fc) `shouldBe` Right fc

spec :: Spec
spec = do
  describe "parseXml_filter" $ do
    it "parses a complete filter" $ do
      levelsXml <- BL.readFile "kpp/levels.xml"
      testParse levelsFilterConfig levelsXml

    it "parses filterconfig version" $
      parses filterVersion exampleXml `shouldBe` Right "2"

    it "fails when version is missing" $
      testParseFailure "<filterconfig></filterconfig>"

    it "parses filterconfig parameters" $
      parses filterParams exampleXml `shouldBe` Right (Map.fromList [("example", Unknown "value")])

  describe "renderXml_filterconfig" $ do
    it "renders a filterconfig element" $
      let fc = FilterConfig "2" $ Map.fromList [("example", Unknown "value")]
          render = elementToLBS'
                   . renderXml_filterconfig
      in render fc `shouldBe` exampleXml

    it "is the inverse of parseXml_filterconfig" $
      testRender levelsFilterConfig
