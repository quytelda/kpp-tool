{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module SpecPNG where

import           Data.Binary.Get
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Test.Hspec

import           PNG

path_1px :: FilePath
path_1px = "png/1px.png"

spec_PNG :: Spec
spec_PNG = do
  describe "isPngData" $ do
    it "recognizes PNG data" $ do
      png <- BS.readFile path_1px
      png `shouldSatisfy` isPngData

    it "recognized invalid PNG data" $ do
      let binData = "\x89\x50\x4E\x47\x0D\x0E\x1A\x0A"
      binData `shouldSatisfy` (not . isPngData)

  describe "getTextChunk" $ do
    it "parses tEXt chunks" $ do
      let bytes = "tEXtkey\x00value"
      result <- runGetOrFail' (getTextChunk "key") bytes
      result `shouldBe` "value"

  describe "getZtxtChunk" $ do
    it "parses zTXt chunks" $ do
      let bytes = "zTXtkey\NUL\NULx\156+K\204)M\ENQ\NUL\ACKj\STX\RS"
      result <- runGetOrFail' (getZtxtChunk "key") bytes
      result `shouldBe` "value"

  describe "getItxtChunk" $ do
    it "parses iTXt chunks" $ do
      pending

  describe "getIhdrDimensions" $ do
    it "gets image dimensions" $ do
      pending

  describe "getChunk" $ do
    it "parses chunks" $ do
      pending

  describe "parseSettingsXml" $ do
    it "parses settings XML" $ do
      pending
