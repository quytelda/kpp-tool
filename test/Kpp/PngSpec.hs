{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Kpp.PngSpec (spec) where

import           Conduit
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as BL
import           Test.Hspec

import           Kpp.Png

ihdrBin :: LazyByteString
ihdrBin = "\NUL\NUL\NUL\rIHDR\NUL\NUL\NUL\SOH\NUL\NUL\NUL\SOH\b\STX\NUL\NUL\NUL\144wS\222"

ihdrChunk :: PngChunk
ihdrChunk = PngChunk "IHDR" "\NUL\NUL\NUL\SOH\NUL\NUL\NUL\SOH\b\STX\NUL\NUL\NUL"

versionChunk :: PngChunk
versionChunk = PngChunk
  { chunkType = "tEXt"
  , chunkData = "version\NUL5.0"
  }

settingChunk :: PngChunk
settingChunk = PngChunk
  { chunkType = "zTXt"
  , chunkData = "preset\NUL\NULx\156\179\t(J-N-\177\179\209\135\&2\NUL5\198\ACK\n"
  }

ignoredChunk :: PngChunk
ignoredChunk = PngChunk
  { chunkType = "tIME"
  , chunkData = "\a\232\v\EM\NUL*\DC4"
  }

spec :: Spec
spec = do
  -- Binary parsers
  describe "getChunk" $ do
    it "unwraps PNG chunks" $ do
      runGet getChunk ihdrBin
        `shouldBe` ihdrChunk

  describe "putChunk" $ do
    it "wraps PNG chunks" $ do
      runPut (putChunk ihdrChunk)
        `shouldBe` ihdrBin

  describe "getTextChunk" $ do
    it "parses tEXt chunks" $ do
      let bytes = "key\x00value"
      runGet getTextChunk bytes
        `shouldBe` ("key", "value")

  describe "putTextChunk" $ do
    it "renders tEXt chunks" $ do
      runPut (putTextChunk "key" "value")
        `shouldBe` "key\NULvalue"

  describe "getZtxtChunk" $ do
    it "parses zTXt chunks" $ do
      let bytes = "key\NUL\NULx\156+K\204)M\ENQ\NUL\ACKj\STX\RS"
      runGet (getZtxtChunk) bytes
        `shouldBe` ("key", "value")

  describe "putZtxtChunk" $ do
    it "renders zTXt chunks" $ do
      runPut (putZtxtChunk "key" "value")
        `shouldBe` "key\NUL\NULx\156+K\204)M\ENQ\NUL\ACKj\STX\RS"

  describe "getItxtChunk" $ do
    it "parses iTXt chunks" $ do
      let bytes = "key\NUL\SOH\NUL\NUL\NULx\156+K\204)M\ENQ\NUL\ACKj\STX\RS"
      runGet (getItxtChunk) bytes
        `shouldBe` ("key", "value")

  describe "putItxtChunk" $ do
    it "renders iTXt chunks" $ do
      runPut (putItxtChunk True "key" "value")
        `shouldBe` "key\NUL\SOH\NUL\NUL\NULx\156+K\204)M\ENQ\NUL\ACKj\STX\RS"

  describe "getIhdrDimensions" $ do
    it "parses dimensions from IHDR chunks" $ do
      runGet getIhdrDimensions (chunkData ihdrChunk)
        `shouldBe` (1, 1)

  -- KPP-specific chunks
  describe "isSpecialChunk" $ do
    it "accepts version chunks" $ do
      versionChunk `shouldSatisfy` isSpecialChunk

    it "accepts setting chunks" $ do
      settingChunk `shouldSatisfy` isSpecialChunk

    it "rejects irrelevant chunks" $ do
      ignoredChunk `shouldSatisfy` (not . isSpecialChunk)

  describe "parseChunk" $ do
    it "parses version chunks" $ do
      parseChunk versionChunk `shouldBe` VersionChunk "5.0"

    it "parses setting chunks" $ do
      parseChunk settingChunk `shouldBe` SettingChunk "<Preset></Preset>"

    it "ignores regular chunks" $ do
      parseChunk ignoredChunk `shouldBe` IgnoredChunk ignoredChunk

  -- Conduits
  describe "unwrapChunks" $ do
    it "finds all chunks" $ do
      let conduit =
            sourceFile "png/1px.png"
            .| unwrapChunks
            .| mapC chunkType
            .| sinkList

      runConduitRes conduit
        `shouldReturn` ["IHDR","iCCP","pHYs","tIME","tEXt","IDAT","IEND"]

  describe "wrapChunks" $ do
    it "inverts unwrapChunks" $ do
      content1 <- BL.readFile "png/1px.png"
      content2 <- runConduitRes
        $ sourceFile "png/1px.png"
        .| unwrapChunks
        .| wrapChunks
        .| sinkLazy
      content1 `shouldBe` content2

  describe "pngDimensions" $ do
    it "parses PNG dimensions" $ do
      runConduitRes (sourceFile "png/1px.png" .| pngDimensions)
        `shouldReturn` (1,1)
