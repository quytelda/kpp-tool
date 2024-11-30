{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Kpp.PngSpec (spec) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Test.Hspec

import           Common
import           Kpp.Png

spec :: Spec
spec = do
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

  describe "putTextChunk" $ do
    it "renders tEXt chunks" $ do
      let result = runPut (putTextChunk "key" "value")
      result `shouldBe` "tEXtkey\NULvalue"

  describe "getZtxtChunk" $ do
    it "parses zTXt chunks" $ do
      let bytes = "zTXtkey\NUL\NULx\156+K\204)M\ENQ\NUL\ACKj\STX\RS"
      result <- runGetOrFail' (getZtxtChunk "key") bytes
      result `shouldBe` "value"

  describe "putZtxtChunk" $ do
    it "renders zTXt chunks" $ do
      let result = runPut (putZtxtChunk "key" "value")
      result `shouldBe` "zTXtkey\NUL\NULx\156+K\204)M\ENQ\NUL\ACKj\STX\RS"

  describe "getItxtChunk" $ do
    it "parses iTXt chunks" $ do
      let bytes = "iTXtkey\NUL\SOH\NUL\NUL\NULx\156+K\204)M\ENQ\NUL\ACKj\STX\RS"
      result <- runGetOrFail' (getItxtChunk "key") bytes
      result `shouldBe` "value"

  describe "putItxtChunk" $ do
    it "renders iTXt chunks" $ do
      let result = runPut (putItxtChunk True "key" "value")
      result `shouldBe` "iTXtkey\NUL\SOH\NUL\NUL\NULx\156+K\204)M\ENQ\NUL\ACKj\STX\RS"

  describe "getIhdrDimensions" $ do
    it "gets image dimensions" $ do
      let bytes = "IHDR\NUL\NUL\NUL\200\NUL\NUL\NUL\200\b\ACK\NUL\NUL\NUL"
      result <- runGetOrFail' getIhdrDimensions bytes
      result `shouldBe` (200, 200)

  describe "getChunk" $ do
    it "parses chunks" $ do
      let bytes = "\NUL\NUL\NUL\rIHDR\NUL\NUL\NUL\SOH\NUL\NUL\NUL\SOH\b\STX\NUL\NUL\NUL\144wS\222"
      result <- runGetOrFail' getChunk bytes
      result `shouldBe` RegularChunk "IHDR\NUL\NUL\NUL\SOH\NUL\NUL\NUL\SOH\b\STX\NUL\NUL\NUL"

  describe "putChunk" $ do
    it "renders chunks" $ do
      let result = runPut $ putChunk (RegularChunk "zTXtkey\NUL\NULx\156+K\204)M\ENQ\NUL\ACKj\STX\RS")
      result `shouldBe` "\NUL\NUL\NUL\DC2zTXtkey\NUL\NULx\156+K\204)M\ENQ\NUL\ACKj\STX\RS\DLE\229\228\253"

  describe "parseSettingsXml" $ do
    it "parses settings XML" $ do
      xml    <- BL.readFile "kpp/basic-ellipse.xml"
      result <- BL.readFile path_basicEllipse >>= parseSettingsXml
      xml `shouldBe` result
