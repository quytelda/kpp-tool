{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Exception
import           Data.Binary
import qualified Data.ByteString.Lazy as BL
import           Data.Int             (Int64)
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import qualified Data.Text            as T
import           System.Directory
import           Test.Hspec

import           App
import           Preset

pngIHDRChunkSize :: Int64
pngIHDRChunkSize = 17

-- | This is the directory where temporary test files will be created
-- during unit tests. It is automatically created when tests are run,
-- then deleted after the tests complete.
testDir :: FilePath
testDir = "test.tmp"

withTestDir :: IO () -> IO ()
withTestDir = bracket_
  (createDirectory testDir)
  (removeDirectoryRecursive testDir)

----------------------------
-- Sample Files for Tests --
----------------------------

path_basicShapeGrainy :: FilePath
path_basicShapeGrainy = "kpp/basic-shape-grainy.kpp"

main :: IO ()
main = withTestDir $ do
  hspec spec_Preset
  hspec spec_App

-----------
-- Tests --
-----------

spec_App :: Spec
spec_App = do
  describe "App" $ do
    spec_FromArgument

spec_Preset :: Spec
spec_Preset = describe "Preset" $ do
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
    fromArgument "key1=value1,key2=value2" `shouldBe` Right (Map.fromList [ ("key1", "value1")
                                                                          , ("key2", "value2")
                                                                          ] :: Map.Map T.Text T.Text)
