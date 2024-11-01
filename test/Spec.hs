{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import qualified Crypto.Hash.MD5      as MD5
import           Data.Binary
import qualified Data.ByteString.Lazy as BL
import           Data.Int             (Int64)
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import qualified Data.Text            as T
import           System.Exit
import           Test.Hspec

import           App
import           Preset

path_basicEllipse :: FilePath
path_basicEllipse = "kpp/basic-ellipse.kpp"

path_basicShapeGrainy :: FilePath
path_basicShapeGrainy = "kpp/basic-shape-grainy.kpp"

main :: IO ()
main = do
  hspec specPreset
  hspec specApp

pngIHDRChunkSize :: Int64
pngIHDRChunkSize = 17

specPreset :: Spec
specPreset = describe "Preset" $ do
    before (decode <$> BL.readFile "kpp/basic-shape-grainy.kpp") $ do
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
        let md5sum = "b877c93efe4540891304ae3662e9ce58"
            brush  = fromJust $ lookupResourceByMD5 md5sum preset

        resourceName brush `shouldBe` "egg"
        resourceFile brush `shouldBe` "egg.png"
        resourceType brush `shouldBe` "brushes"
        resourceMD5  brush `shouldBe` md5sum

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

specApp :: Spec
specApp = do
  describe "App" $ do
    describe "start" $ do
      it "can save presets to file" $ do
        start ["--output=out.kpp", path_basicEllipse]

        preset1 <- loadPreset path_basicEllipse
        preset2 <- loadPreset "out.kpp"

        preset1 `shouldBe` preset2

      it "can change preset names" $ do
        start [ "--set-name", "new_name"
              , "--output", "out.kpp"
              , path_basicEllipse
              ]

        Preset{..} <- loadPreset "out.kpp"
        presetName `shouldBe` "new_name"

      it "can set parameter values" $ do
        start [ "--set-param", "EraserMode=string:true"
              , "--set-param", "ExampleParam=binary:ZXhhbXBsZQ=="
              , "--output", "out.kpp"
              , path_basicEllipse
              ]

        preset <- loadPreset "out.kpp"
        lookupParam "EraserMode"   preset `shouldBe` Just (String "true")
        lookupParam "ExampleParam" preset `shouldBe` Just (Binary "example")

      it "can extract embedded resources" $ do
        start [ "--extract", "name=egg"
              , "--extract", "md5=3ca1bcf8dc1bc90b5a788d89793d2a89"
              , "kpp/basic-shape-grainy.kpp"]

        egg       <- BL.readFile "egg.png"
        hourglass <- BL.readFile "hourglass.png"

        MD5.hashlazy egg       `shouldBe` "\184w\201>\254E@\137\DC3\EOT\174\&6b\233\206X"
        MD5.hashlazy hourglass `shouldBe` "<\161\188\248\220\ESC\201\vZx\141\137y=*\137"

      it "extracts all resources" $ do
        pending

      it "can extract a preset icon" $ do
        start [ "--get-icon", "out.png", path_basicEllipse ]

        icon <- BL.readFile "out.png"
        MD5.hashlazy icon `shouldBe` "\156\170\178\231n\230\\Fx\153 \235)\173\155N"

      it "can set a preset icon" $ do
        start [ "--set-icon", "egg.png"
              , "--output", "out.kpp"
              , path_basicEllipse]

        egg <- BL.readFile "egg.png"
        preset <- loadPreset "out.kpp"
        getPresetIcon preset `shouldBe` egg

      -- it "nothing" $ do
      --   start ["--sldkjfsdl"] `shouldThrow` (== ExitFailure 1)

    describe "FromArgument" $ do
      it "can parse String arguments" $ do
        fromArgument "example" `shouldBe` Right ("example" :: String)

      it "can parse Text arguments" $ do
        fromArgument "example" `shouldBe` Right ("example" :: T.Text)

      it "can parse string ParamValue arguments" $ do
        fromArgument "string:example" `shouldBe` Right (String "example")

      it "can parse internal ParamValue arguments" $ do
        fromArgument "internal:example" `shouldBe` Right (Internal "example")

      it "can parse binary ParamValue arguments" $ do
        fromArgument "binary:ZXhhbXBsZQ==" `shouldBe` Right (Binary "example")

      it "can parse KEY=VALUE arguments" $ do
        fromArgument "key=value" `shouldBe` Right ("key" :: T.Text, "value" :: T.Text)

      it "can parse dictionary arguments" $ do
        fromArgument "key1=value1,key2=value2" `shouldBe` Right (Map.fromList [ ("key1", "value1")
                                                                              , ("key2", "value2")
                                                                              ] :: Map.Map T.Text T.Text)
