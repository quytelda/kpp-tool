{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Kpp.AppSpec (spec) where

import qualified Crypto.Hash.MD5      as MD5
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Either
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import           System.Directory
import           System.FilePath
import           Test.Hspec

import           Common
import           Kpp.App
import           Kpp.Preset

spec_FromArgument :: Spec
spec_FromArgument = describe "FromArgument" $ do
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
    fromArgument "key1=value1,key2=has\\,commas\\,,key3=value3" `shouldBe`
      Right (Map.fromList [ ("key1", "value1")
                          , ("key2", "has,commas,")
                          , ("key3", "value3")
                          ] :: Map.Map T.Text T.Text)

  context "when provided with invalid input" $ do
    it "returns an error message" $ do
      (fromArgument "invalid:example" :: Either String ParamValue)
        `shouldSatisfy` isLeft
      (fromArgument "invalid" :: Either String (T.Text, T.Text))
        `shouldSatisfy` isLeft
      (fromArgument "k1=v1,invalid,k2=v2" :: Either String (Map.Map T.Text T.Text))
        `shouldSatisfy` isLeft

spec_start :: Spec
spec_start = do
  describe "start" $ do
    it "runs operations in order" $ do
      let file1 = testDir </> "order1.kpp"
          file2 = testDir </> "order2.kpp"
          file3 = testDir </> "order3.kpp"
      start [ "--set-param", "example=string:1"
            , "--output", file1
            , "--set-param", "example=string:2"
            , "--output", file2
            , "--set-param", "example=string:3"
            , "--output", file3
            , path_basicEllipse
            ]

      preset1 <- loadPreset file1
      preset2 <- loadPreset file2
      preset3 <- loadPreset file3

      lookupParam "example" preset1 `shouldBe` Just (String "1")
      lookupParam "example" preset2 `shouldBe` Just (String "2")
      lookupParam "example" preset3 `shouldBe` Just (String "3")

    it "can save presets to file" $ do
      let file = testDir </> "save_presets.kpp"
      start [ "--output", file
            , path_basicEllipse
            ]

      preset1 <- loadPreset path_basicEllipse
      preset2 <- loadPreset file

      preset1 `shouldBe` preset2

    it "can set preset names" $ do
      let file = testDir </> "set_name.kpp"
      start [ "--set-name", "new_name"
            , "--output", file
            , path_basicEllipse
            ]

      Preset{..} <- loadPreset file
      presetName `shouldBe` "new_name"

    it "can synchronize preset names with filenames" $ do
      let file = testDir </> "sync_name.kpp"
      start [ "--sync-name"
            , "--output", file
            , path_basicEllipse
            ]

      Preset{..} <- loadPreset file
      presetName `shouldBe` "basic-ellipse"

    it "can set parameter values" $ do
      let file = testDir </> "set_param.kpp"
      start [ "--set-param", "EraserMode=string:true"
            , "--set-param", "ExampleParam=binary:ZXhhbXBsZQ=="
            , "--output", file
            , path_basicEllipse
            ]

      preset <- loadPreset file
      lookupParam "EraserMode"   preset `shouldBe` Just (String "true")
      lookupParam "ExampleParam" preset `shouldBe` Just (Binary "example")

    it "can extract embedded resources" $ do
      let eggFile       = testDir </> "extract_egg.png"
          hourglassFile = testDir </> "extract_hourglass.png"
      start [ "--extract", "name=egg,path=" <> eggFile
            , "--extract", "md5=3ca1bcf8dc1bc90b5a788d89793d2a89,path=" <> hourglassFile
            , path_basicShapeGrainy]

      egg       <- BL.readFile eggFile
      hourglass <- BL.readFile hourglassFile

      MD5.hashlazy egg       `shouldBe` "\184w\201>\254E@\137\DC3\EOT\174\&6b\233\206X"
      MD5.hashlazy hourglass `shouldBe` "<\161\188\248\220\ESC\201\vZx\141\137y=*\137"

    it "can extract all resources" $ do
      let destDir = testDir </> "extract_all"
      createDirectory destDir
      start [ "--extract-all=" <> destDir
            , path_basicShapeGrainy ]

      egg       <- BL.readFile $ destDir </> "egg.png"
      hourglass <- BL.readFile $ destDir </> "hourglass.png"

      MD5.hashlazy egg       `shouldBe` "\184w\201>\254E@\137\DC3\EOT\174\&6b\233\206X"
      MD5.hashlazy hourglass `shouldBe` "<\161\188\248\220\ESC\201\vZx\141\137y=*\137"

    it "can embed resources" $ do
      let file = testDir </> "embed_resource.kpp"
      start [ "--embed", "name=scribble,type=brushes,file=scribble.png,path=" <> path_scribble
            , "--output", file
            , path_basicEllipse ]

      preset <- loadPreset file
      scribble <- Resource "scribble" "scribble.png" "brushes" <$> BS.readFile path_scribble
      lookupResourceByName "scribble" preset `shouldBe` Just scribble

    it "can extract preset icons" $ do
      let file = testDir </> "extract_icon.png"
      start [ "--get-icon", file
            , path_basicEllipse ]

      icon <- BL.readFile file
      MD5.hashlazy icon `shouldBe` "\156\170\178\231n\230\\Fx\153 \235)\173\155N"

    it "can set preset icons" $ do
      let file = testDir </> "set_icon.kpp"
      start [ "--set-icon", path_scribble
            , "--output", file
            , path_basicEllipse]

      egg <- BL.readFile path_scribble
      preset <- loadPreset file
      getPresetIcon preset `shouldBe` egg

spec :: Spec
spec = do
  describe "App" $ do
    spec_FromArgument
    spec_start

