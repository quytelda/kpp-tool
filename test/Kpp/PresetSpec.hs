{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Kpp.PresetSpec (spec) where

import           Data.Binary
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import           Test.Hspec

import           Common
import           Kpp.Preset

spec :: Spec
spec = describe "Preset" $ do
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

  describe "parseSettingsXml" $ do
    it "can extract XML settings" $ do
      bytes <- BL.readFile path_basicEllipse
      xml   <- parseSettingsXml bytes
      md5sum (BL.toStrict xml) `shouldBe` "2f2715bf7fca5349608f061626e73ac2"

  before (decode <$> BL.readFile path_basicShapeGrainy) $ do
    specify "can parse preset version" $ \Preset{..} ->
      presetVersion `shouldBe` "5.0"

    specify "can parse preset names" $ \Preset{..} ->
      presetName `shouldBe` "e) Basic Shape Grainy"

    specify "can parse preset paintopid" $ \Preset{..} ->
      presetPaintop `shouldBe` "paintbrush"

    specify "can parse preset parameters" $ \Preset{..} -> do
      length presetParams `shouldBe` 225

      Map.lookup "ColorSource/Type" presetParams `shouldBe` Just (String   "plain")
      Map.lookup "EraserMode"       presetParams `shouldBe` Just (Internal "false")

    specify "can parse filter settings" $ \preset -> do
      presetFilter preset `shouldBe` Nothing

      let params =
            [ ("blackvalue",         Internal "7")
            , ("channel_0",          Unknown "0;1;1;0;1")
            , ("channel_1",          Unknown "0;1;1;0;1")
            , ("channel_2",          Unknown "0;1;1;0;1")
            , ("channel_3",          Unknown "0;1;1;0;1")
            , ("channel_4",          Unknown "0;1;1;0;1")
            , ("channel_5",          Unknown "0;1;1;0;1")
            , ("channel_6",          Unknown "0;1;1;0;1")
            , ("channel_7",          Unknown "0;1;1;0;1")
            , ("gammavalue",         Internal "0.681020988537474")
            , ("histogram_mode",     Unknown "linear")
            , ("lightness",          Unknown "0.0277777777777778;1;0.681020988537474;\
                                             \0.0925925925925926;0.580246913580247")
            , ("mode",               Unknown "lightness")
            , ("number_of_channels", Unknown "8")
            , ("outblackvalue",      Internal "24")
            , ("outwhitevalue",      Internal "148")
            , ("whitevalue",         Internal "255")
            ]
          filterConfig = FilterConfig "2" (Map.fromList params)

      levelsKpp <- loadPreset path_levels
      presetFilter levelsKpp `shouldBe` Just filterConfig

    specify "can render filter settings" $ \_ -> do
      pending

    specify "can parse embedded resources" $ \Preset{..} -> do
      -- The example has 2 embedded resources: a brush and a pattern.
      length embeddedResources `shouldBe` 2

      embeddedResources `shouldSatisfy` Map.member "egg"
      embeddedResources `shouldSatisfy` Map.member "hourglass.png"

    specify "can parse preset icons" $ \Preset{..} -> do
      let ihdrChunk = head presetIcon
          iendChunk = last presetIcon
          width     = decode (BL.take 4 $ BL.drop 4 ihdrChunk) :: Word32
          height    = decode (BL.take 4 $ BL.drop 8 ihdrChunk) :: Word32

      -- The example preset contains 6 regular chunks.
      length presetIcon `shouldBe` 6

      BL.take 4 ihdrChunk `shouldBe` "IHDR"
      BL.length ihdrChunk `shouldBe` pngIHDRChunkSize
      width  `shouldBe` 200
      height `shouldBe` 200

      iendChunk `shouldBe` "IEND"

    specify "can lookup resources by MD5 checksum" $ \preset -> do
      let csum  = "b877c93efe4540891304ae3662e9ce58"
          brush = fromJust $ lookupResourceByMD5 csum preset

      resourceName brush `shouldBe` "egg"
      resourceFile brush `shouldBe` "egg.png"
      resourceType brush `shouldBe` "brushes"
      resourceMD5  brush `shouldBe` csum

    specify "can be encoded as a lazy ByteString" $ \preset -> do
      (decode . encode) preset `shouldBe` preset

    specify "can change a preset icon" $ \preset -> do
      scribble   <- BL.readFile "kpp/basic-ellipse.kpp"
      Preset{..} <- either fail pure $ setPresetIcon scribble preset
      let ihdrChunk = head presetIcon
          iendChunk = last presetIcon
          width     = decode (BL.take 4 $ BL.drop 4 ihdrChunk) :: Word32
          height    = decode (BL.take 4 $ BL.drop 8 ihdrChunk) :: Word32

      length presetIcon `shouldBe` 5

      BL.take 4 ihdrChunk `shouldBe` "IHDR"
      BL.length ihdrChunk `shouldBe` pngIHDRChunkSize
      width  `shouldBe` 200
      height `shouldBe` 200

      iendChunk `shouldBe` "IEND"
