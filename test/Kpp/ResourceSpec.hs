{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Kpp.ResourceSpec (spec) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Either
import           Test.Hspec

import           Common
import           Kpp.Resource

exampleXml :: BL.ByteString
exampleXml = "<resource filename=\"file\" md5sum=\"b5bac05f70c47a9b4fafab2db649827e\" name=\"name\" type=\"patterns\">ARA=</resource>"

parses :: (Resource -> a) -> BL.ByteString -> Either String a
parses f = fmap f . parseXml_resource . parseXmlElement

testParseFailure :: BL.ByteString -> Expectation
testParseFailure xml = parses id xml `shouldSatisfy` isLeft

testRender :: Resource -> Expectation
testRender res = parseXml_resource (renderXml_resource res) `shouldBe` Right res

exampleResource :: Resource
exampleResource = Resource "name" "file" "patterns" (BS.pack [1,16])

spec :: Spec
spec = do
  describe "resourceMD5" $ do
    it "computes a resource's MD5 checksum" $
      resourceMD5 exampleResource `shouldBe` "b5bac05f70c47a9b4fafab2db649827e"

  describe "parseXml_resource" $ do
    it "parses resource name" $
      parses resourceName exampleXml `shouldBe` Right "name"

    it "parses resource type" $
      parses resourceType exampleXml `shouldBe` Right "patterns"

    it "fails when type is missing" $
      testParseFailure "<resource filename=\"file\" md5sum=\"b5bac05f70c47a9b4fafab2db649827e\" name=\"name\">ARA=</resource>"

    it "validates MD5 checksum" $
      testParseFailure "<resource filename=\"file\" md5sum=\"2bda2998d9b0ee197da142a0447f6725\" name=\"name\" type=\"patterns\">ARA=</resource>"

    it "parses resource filename" $
      parses resourceFile exampleXml `shouldBe` Right "file"

  describe "renderXml_resource" $ do
    it "renders a resource element" $
      let render = elementToLBS' . renderXml_resource
      in render exampleResource `shouldBe` exampleXml

    it "is the inverse of parseXml_resource" $
      testRender exampleResource

  describe "loadResource" $ do
    it "loads resources from file" $ do
      resource <- loadResource "png/scribble.png" "patterns" (Just "name") (Just "file")
      png <- BS.readFile "png/scribble.png"

      resource `shouldBe` Resource "name" "file" "patterns" png

  describe "saveResource" $ do
    it "saves resources to file" $ do
      pending
      -- png <- BS.readFile "png/scribble.png"
      -- let resource = Resource "name" "file" "patterns" png
