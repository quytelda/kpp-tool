{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Data.Binary
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import           Test.Hspec

import           Preset

main :: IO ()
main = hspec specPreset

specPreset :: Spec
specPreset = describe "Preset" $ do
    before (decode <$> BL.readFile "kpp/basic-shape-grainy.kpp") $ do
      specify "can parse preset version" $ \Preset{..} ->
        presetVersion `shouldBe` "5.0"

      specify "can parse preset names" $ \Preset{..} ->
        presetName `shouldBe` "e) Basic Shape Grainy"

      specify "can parse preset parameters" $ \Preset{..} -> do
        length presetParams `shouldBe` 225

        Map.lookup "ColorSource/Type" presetParams `shouldBe` Just (String   "plain")
        Map.lookup "EraserMode"       presetParams `shouldBe` Just (Internal "false")

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
        BL.length ihdrChunk `shouldBe` 17 -- per PNG specification
        width  `shouldBe` 200
        height `shouldBe` 200

        iendChunk `shouldBe` "IEND"

      specify "can lookup resources by MD5 checksum" $ \preset -> do
        let md5sum = "b877c93efe4540891304ae3662e9ce58"
            brush  = fromJust $ lookupResourceByMD5 md5sum preset

        resourceName brush `shouldBe` "egg"
        resourceFile brush `shouldBe` "egg.png"
        resourceType brush `shouldBe` "brushes"
        resourceMD5  brush `shouldBe` md5sum

      specify "can be encoded as a lazy ByteString" $ \preset -> do
        (decode . encode) preset `shouldBe` preset
