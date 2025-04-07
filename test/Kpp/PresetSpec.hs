{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Kpp.PresetSpec (spec) where

import           Data.Binary
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import           System.FilePath
import           Test.Hspec

import           Common
import           Kpp.Common
import           Kpp.Param
import           Kpp.Png
import           Kpp.Preset
import           Kpp.Resource

spec :: Spec
spec = do
  describe "parseSettingsXml" $ do
    it "can extract XML settings" $ do
      bytes <- BL.readFile path_basicEllipse
      xml   <- parseSettingsXml bytes
      md5sum (BL.toStrict xml) `shouldBe` "2f2715bf7fca5349608f061626e73ac2"

  before (decode <$> BL.readFile path_basicShapeGrainy) $ do
    describe "getPreset" $ do
      it "parses preset version" $ \Preset{..} ->
        presetVersion `shouldBe` "5.0"

      it "parses preset name" $ \Preset{..} ->
        presetName `shouldBe` "e) Basic Shape Grainy"

      it "parses preset paintopid" $ \Preset{..} ->
        presetPaintop `shouldBe` "paintbrush"

      it "parses preset parameters" $ \Preset{..} -> do
        Map.size presetParams `shouldBe` 225

        -- We only check a few parameters to ensure things are working.
        -- TODO: Test a parameter of each param type.
        Map.lookup "ColorSource/Type" presetParams `shouldBe` Just (String   "plain")
        Map.lookup "EraserMode"       presetParams `shouldBe` Just (Internal "false")

      it "parses filter configurations" $ \preset -> do
        -- This preset has no filter.
        presetFilter preset `shouldBe` Nothing

        -- Try a preset that actually has a filter.
        levelsKpp <- loadPreset path_levels
        presetFilter levelsKpp `shouldBe` Just levelsFilterConfig

      it "parses embedded resources" $ \Preset{..} -> do
        Map.size embeddedResources `shouldBe` 2

        let getMD5 rname = resourceMD5 <$> Map.lookup rname embeddedResources
        getMD5 "egg"           `shouldBe` Just "b877c93efe4540891304ae3662e9ce58"
        getMD5 "hourglass.png" `shouldBe` Just "3ca1bcf8dc1bc90b5a788d89793d2a89"

      it "parses preset icon" $ \Preset{..} -> do
        length presetIcon `shouldBe` 6

        let ihdrChunk = head presetIcon
            iendChunk = last presetIcon
            width     = decode (BL.take 4 $ BL.drop 4 ihdrChunk) :: Word32
            height    = decode (BL.take 4 $ BL.drop 8 ihdrChunk) :: Word32

        BL.take 4 ihdrChunk `shouldBe` "IHDR"
        BL.length ihdrChunk `shouldBe` pngIHDRChunkSize
        width  `shouldBe` 200
        height `shouldBe` 200

        iendChunk `shouldBe` "IEND"

    -- End: getPreset

    describe "putPreset" $ do
      it "encodes a preset as a ByteString" $ \preset -> do
        (decode . encode) preset `shouldBe` preset

      it "encodes filter configs correctly" $ \_ -> do
        let file = testDir </> "render_filter.kpp"
        loadPreset path_levels >>= savePreset file
        renderFilterKpp <- loadPreset file
        presetFilter renderFilterKpp `shouldBe` Just levelsFilterConfig

    describe "lookupResourceByMD5" $ do
      it "finds a resource by MD5 checksum" $ \preset -> do
        let csum  = "b877c93efe4540891304ae3662e9ce58"
            brush = fromJust $ lookupResourceByMD5 csum preset

        resourceName brush `shouldBe` "egg"
        resourceFile brush `shouldBe` "egg.png"
        resourceType brush `shouldBe` "brushes"
        resourceMD5  brush `shouldBe` csum

    describe "setPresetIcon" $ do
      it "changes a preset icon" $ \preset -> do
        ellipse    <- BL.readFile "kpp/basic-ellipse.kpp"
        Preset{..} <- either fail pure $ setPresetIcon ellipse preset

        length presetIcon `shouldBe` 5

        let ihdrChunk = head presetIcon
            iendChunk = last presetIcon
            width     = decode (BL.take 4 $ BL.drop 4 ihdrChunk) :: Word32
            height    = decode (BL.take 4 $ BL.drop 8 ihdrChunk) :: Word32

        BL.take 4 ihdrChunk `shouldBe` "IHDR"
        BL.length ihdrChunk `shouldBe` pngIHDRChunkSize
        width  `shouldBe` 200
        height `shouldBe` 200

        iendChunk `shouldBe` "IEND"

    describe "presetIconDimensions" $ do
      it "returns preset icon dimensions" $ \preset ->
        presetIconDimensions preset `shouldBe` (200, 200)

  describe "loadPreset" $ do
    it "reads presets from file" $ do
      Preset{..} <- loadPreset path_basicShapeGrainy

      presetVersion `shouldBe` "5.0"
      presetName    `shouldBe` "e) Basic Shape Grainy"
      presetPaintop `shouldBe` "paintbrush"
      presetFilter  `shouldBe` Nothing

      length presetParams `shouldBe` 225
      length presetIcon   `shouldBe` 6
