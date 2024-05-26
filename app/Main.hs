{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Arrow
import           Control.Monad
import           Data.Binary
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Base64    as Base64
import qualified Data.ByteString.Char8     as Char8
import           Data.Functor
import           Data.Maybe                (listToMaybe)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Prettyprinter
import           Prettyprinter.Render.Text
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

import           Preset

-- | An `Action` is a function that (possibly) modifies a preset in the
-- IO monad. For example `showPreset` prints a preset description to
-- the console while passing the preset through unchanged. `setName`
-- updates the name of the preset.
--
-- An action is just a Kleisli arrow of the form @Preset -> IO
-- Preset@, so they are composable using `>>=` and `>=>`. Most command
-- line switches correspond to a particular action, so the full
-- command line arguments passed to the program represents a chain of
-- sequenced `Action`s.
type Action = Preset -> IO Preset

showPreset :: Action
showPreset preset = putDoc (pretty preset) *> putChar '\n' $> preset

getName :: Action
getName preset = TIO.putStrLn (presetName preset) $> preset

setName :: String -> Action
setName name = pure . setPresetName (T.pack name)

getParam :: String -> Action
getParam key preset = preset <$
  case lookupParam (T.pack key) preset of
    Just val -> putDoc (pretty val) *> putChar '\n'
    Nothing  -> error $ "no such parameter: " <> key

setParamString :: String -> Action
setParamString arg preset = do
  (key, val) <- keyEqualsValue arg
  return $ insertParam (T.pack key) (String $ T.pack val) preset

setParamInternal :: String -> Action
setParamInternal arg preset = do
  (key, val) <- keyEqualsValue arg
  return $ insertParam (T.pack key) (Internal $ T.pack val) preset

setParamBinary :: String -> Action
setParamBinary arg preset = do
  (key, encodedVal) <- keyEqualsValue arg
  decodedVal <- either fail pure $ Base64.decode $ Char8.pack encodedVal
  return $ insertParam (T.pack key) (Binary decodedVal) preset

writeResource :: Resource -> IO ()
writeResource Resource{..} = do
  TIO.putStrLn $ "--> Writing resource data to: " <> resourceFile
  BS.writeFile (T.unpack resourceFile) resourceData

extract :: String -> Action
extract name preset = preset <$
  case lookupResource (T.pack name) preset of
    Just res -> writeResource res
    Nothing  -> error $ "No such resource: " <> name

insert :: String -> Action
insert = error "Not implemented yet."

extractAll :: Action
extractAll preset = preset <$ mapM_ writeResource (embeddedResources preset)

-- | Helper function for decoding presets
decoder :: Applicative f => BS.ByteString -> f Preset
decoder = pure . decode . BS.fromStrict

-- | Helper function for encoding presets
encoder :: Applicative f => Preset -> f BS.ByteString
encoder = pure . BS.toStrict . encode

-- | Discard is a data sink which discards any input sent to it.
discard :: Applicative f => a -> f ()
discard = const $ pure ()

compose :: [Action] -> Action
compose = foldr1 (>=>)

----------------------
-- Argument Parsing --
----------------------

breakOn :: Eq a => a -> [a] -> ([a], [a])
breakOn c = second (drop 1) . break (== c)

commaSep :: String -> [String]
commaSep "" = []
commaSep xs = uncurry (:) . second commaSep . breakOn ',' $ xs

keyEqualsValue :: MonadFail m => String -> m (String, String)
keyEqualsValue cs | '=' `elem` cs = pure $ breakOn '=' cs
                  | otherwise     = fail "expecting KEY=VALUE"

data Config = Config
  { configHelp    :: Bool
  , configVersion :: Bool
  , configInput   :: Maybe FilePath
  , configOutput  :: Maybe FilePath
  , configActions :: [Action]
  }

defaults :: Config
defaults = Config
  { configHelp    = False
  , configVersion = False
  , configInput   = Nothing
  , configOutput  = Nothing
  , configActions = []
  }

addAction :: Action -> Config -> Config
addAction action config@Config{..} = config { configActions = action : configActions }

addFileSink :: FilePath -> Config -> Config
addFileSink path config = config {configOutput = Just path}

setInput :: FilePath -> Config -> Config
setInput path config = config {configInput = Just path}

options :: [OptDescr (Config -> Config)]
options = [ Option "h" ["help"]
            (NoArg $ \c -> c {configHelp = True})
            "Display help and usage information."
          , Option "v" ["version"]
            (NoArg $ \c -> c {configVersion = True})
            "Display version information."
          , Option "o" ["output"]
            (ReqArg addFileSink "FILE")
            "Write preset data to FILE.\nOverrides -i/--in-place."
          , Option "i" ["in-place"]
            (NoArg $ \c -> c {configOutput = configInput c})
            "Modify a preset file in-place.\nOverrides -o/--output."

          -- Actions
          , Option "s" ["show"]
            (NoArg $ addAction showPreset)
            "Print preset information."
          , Option "" ["get-name"]
            (NoArg $ addAction getName)
            "Print the preset's metadata name."
          , Option "" ["set-name"]
            (ReqArg (addAction . setName) "STRING")
            "Change a preset's metadata name."
          , Option "p" ["get-param"]
            (ReqArg (addAction . getParam) "KEY")
            "Print the value of a parameter."
          , Option "" ["set-param-string"]
            (ReqArg (addAction . setParamString) "KEY=VALUE")
            "Set the value of parameter."
          , Option "" ["set-param-internal"]
            (ReqArg (addAction . setParamInternal) "KEY=VALUE")
            "Set the value of parameter."
          , Option "" ["set-param-binary"]
            (ReqArg (addAction . setParamBinary) "KEY=VALUE")
            "(Not Implemented) Set the value of parameter."
          , Option "e" ["extract"]
            (ReqArg (addAction . extract) "NAME")
            "Extract an embedded resource."
          , Option "" ["extract-all"]
            (NoArg $ addAction extractAll)
            "Extract all embedded resources."
          , Option "" ["insert"]
            (ReqArg (addAction . insert) "FILE")
            "(Not Implemented) Insert or update a resource file."
          ]

main :: IO ()
main = do
  (flags, args, errs) <- getOpt RequireOrder options <$> getArgs

  unless (null errs) $ do
    hPutStr stderr $ unlines errs
    exitFailure

  let mpath      = listToMaybe args
      Config{..} = foldr (.) (maybe id setInput mpath) flags defaults

  when configHelp $ do
    putStrLn $ usageInfo "kpp-tool" options
    exitSuccess

  when configVersion $ do
    putStrLn "not implemented yet"
    exitSuccess

  let source = maybe BS.getContents BS.readFile  configInput
      sink   = maybe discard        BS.writeFile configOutput

  source
    >>= decoder
    >>= compose configActions
    >>= encoder
    >>= sink
