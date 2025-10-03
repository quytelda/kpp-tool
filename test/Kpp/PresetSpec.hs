{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Kpp.PresetSpec (spec) where

import           Conduit
import qualified Data.Map.Strict as Map
import           Test.Hspec

import           Kpp.Param
import           Kpp.Png
import           Kpp.Preset
import           Kpp.Resource

spec :: Spec
spec = do
  describe "pngToPreset" $ do
    beforeAll (runConduitRes $ sourceFile "kpp/basic-shape-grainy.kpp" .| pngToPreset) $ do
      it "parses preset version" $ \preset -> do
        presetVersion preset `shouldBe` "5.0"

      it "parses preset name" $ \preset -> do
        presetName preset `shouldBe` "e) Basic Shape Grainy"

      it "parses preset paintop" $ \preset -> do
        presetPaintop preset `shouldBe` "paintbrush"

      it "parses preset params" $ \preset -> do
        lookupParam "ColorSource/Type" preset
          `shouldBe` Just (String "plain")

        lookupParam "vcurveMode" preset
          `shouldBe` Just (Internal "0")

        lookupParam "MaskingBrush/Preset/brush_definition" preset
          `shouldBe` Just (String "<Brush randomness=\"0\" density=\"1\" \
                                  \autoSpacingCoeff=\"1\" useAutoSpacing=\"0\" \
                                  \type=\"auto_brush\" BrushVersion=\"2\" \
                                  \angle=\"0\" spacing=\"0.1\"> \
                                  \<MaskGenerator vfade=\"0.5\" \
                                  \antialiasEdges=\"0\" id=\"default\" \
                                  \diameter=\"1.56247\" spikes=\"2\" \
                                  \type=\"circle\" ratio=\"1\" hfade=\"0.5\"/> \
                                  \</Brush> "
                          )

      it "parses embedded resources" $ \preset -> do
        resourceMD5 <$> embeddedResources preset
          `shouldBe` Map.fromList [ ("egg", "b877c93efe4540891304ae3662e9ce58")
                                  , ("hourglass.png", "3ca1bcf8dc1bc90b5a788d89793d2a89")
                                  ]

      it "parses preset icon" $ \preset -> do
        runConduit (sourceLazy (presetIcon preset)
                    .| unwrapChunks
                    .| mapC chunkType
                    .| sinkList
                   )
          `shouldReturn` ["IHDR","pHYs","IDAT","IDAT","IDAT","IEND"]
