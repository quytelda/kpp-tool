{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Binary
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Base64    as Base64
import qualified Data.ByteString.Lazy      as BL
import           Data.Functor
import           Data.Maybe
import qualified Data.Text                 as T
import           Data.Text.Encoding
import qualified Data.Text.IO              as TIO
import           Prettyprinter
import           Prettyprinter.Render.Text
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.FilePath
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

writePreset :: FilePath -> Action
writePreset path preset = preset <$ do
  encoder preset >>= BS.writeFile path

showPreset :: Action
showPreset preset = putDoc (pretty preset) *> putChar '\n' $> preset

getName :: Action
getName preset = TIO.putStrLn (presetName preset) $> preset

setName :: String -> Action
setName name = pure . setPresetName (T.pack name)

syncName :: Config -> Action
syncName Config{..} preset =
  case configInput of
    Just path -> setName (takeBaseName path) preset
    Nothing   -> fail "sync-name: input must be a file path"

getParam :: String -> Action
getParam key preset = preset <$
  case lookupParam (T.pack key) preset of
    Just val -> putDoc (pretty val) *> putChar '\n'
    Nothing  -> error $ "no such parameter: " <> key

setParamString :: String -> Action
setParamString arg preset = do
  (key, val) <- keyEqualsValue arg
  return $ insertParam key (String val) preset

setParamInternal :: String -> Action
setParamInternal arg preset = do
  (key, val) <- keyEqualsValue arg
  return $ insertParam key (Internal val) preset

setParamBinary :: String -> Action
setParamBinary arg preset = do
  (key, encodedVal) <- keyEqualsValue arg
  decodedVal <- either fail pure $ Base64.decode $ encodeUtf8 encodedVal
  return $ insertParam key (Binary decodedVal) preset

writeResource :: Maybe FilePath -> Resource -> IO ()
writeResource mpath Resource{..} = do
  putStrLn $ "==> Writing resource data to: " <> path
  BS.writeFile path resourceData
  where
    path = fromMaybe (T.unpack resourceFile) mpath

extract :: String -> Action
extract arg preset = preset <$
  case mresource of
    Nothing       -> fail "extract: no matching resource found"
    Just resource -> writeResource (T.unpack <$> lookup "path" opts) resource
  where
    opts      = commaSep arg >>= keyEqualsValue
    mresource = (lookup "name" opts >>= flip lookupResourceByName preset) <|>
                (lookup "file" opts >>= flip lookupResourceByFile preset) <|>
                (lookup "md5"  opts >>= flip lookupResourceByMD5  preset)

insert :: String -> Action
insert arg preset = do
  -- TODO: This can probably be simplified by using a second internal
  -- "do" block and constructing the Resource using RecordWildCards.
  rData <- traverse BS.readFile (T.unpack <$> rPath)
  case Resource <$> rName <*> rFile <*> rType <*> rData of
    Just resource -> return $ insertResource resource preset
    Nothing       -> fail "insert: invalid resource definition"
  where
    opts = commaSep arg >>= keyEqualsValue
    rPath = lookup "path" opts <|> lookup "file" opts
    rType = lookup "type" opts
    rName = lookup "name" opts <|> rPath
    rFile = lookup "file" opts <|> rPath

extractAll :: Action
extractAll preset = preset <$
  mapM_ (writeResource Nothing) (embeddedResources preset)

getIcon :: FilePath -> Action
getIcon path preset = preset <$ BL.writeFile path (getPresetIcon preset)

setIcon :: FilePath -> Action
setIcon path preset = do
  pngData <- BL.readFile path
  case setPresetIcon pngData preset of
    Left  err     -> fail $ "set-icon: " <> err
    Right preset' -> return preset'

-- | Helper function for decoding presets
decoder :: Applicative f => BS.ByteString -> f Preset
decoder = pure . decode . BS.fromStrict

-- | Helper function for encoding presets
encoder :: Applicative f => Preset -> f BS.ByteString
encoder = pure . BS.toStrict . encode

-- | Discard is a data sink which discards any input sent to it.
discard :: Applicative f => a -> f ()
discard = const $ pure ()

-- | Helper function to compose a sequence of `Actions`.
compose :: [Action] -> Action
compose = foldr1 (>=>)

----------------------
-- Argument Parsing --
----------------------

-- | Split on the first occurrence of some value.
breakOn :: Eq a => a -> [a] -> ([a], [a])
breakOn c = second (drop 1) . break (== c)

-- | Parse comma separated strings.
commaSep :: String -> [String]
commaSep [] = []
commaSep xs = uncurry (:) . second commaSep . breakOn ',' $ xs

-- | Try to parse a key-value string in "KEY=VALUE" format.
keyEqualsValue :: MonadFail m => String -> m (T.Text, T.Text)
keyEqualsValue cs | '=' `elem` cs = pure $ bimap T.pack T.pack $ breakOn '=' cs
                  | otherwise     = fail "expecting KEY=VALUE"

-- | `Config` represents runtime configuration options.
data Config = Config
  { configHelp    :: Bool
  , configVersion :: Bool
  , configInput   :: Maybe FilePath
  , configInPlace :: Bool
  , configActions :: [Action]
  }

-- | Default runtime configuration options.
defaults :: Config
defaults = Config
  { configHelp    = False
  , configVersion = False
  , configInput   = Nothing
  , configInPlace = False
  , configActions = []
  }

-- | Helper function to add an `Action` to the execution sequence in a Config.
addAction :: Action -> Config -> Config
addAction action config@Config{..} = config { configActions = action : configActions }

-- | Helper function to set the configuration input file.
setInput :: FilePath -> Config -> Config
setInput path config = config {configInput = Just path}

options :: [OptDescr (Config -> Config)]
options = [ Option "h" ["help"]
            (NoArg $ \c -> c {configHelp = True})
            "Display help and usage information."
          , Option "v" ["version"]
            (NoArg $ \c -> c {configVersion = True})
            "Display version information."
          , Option "i" ["in-place"]
            (NoArg $ \c -> c {configInPlace = True})
            "Modify a preset file in-place."

          -- Actions
          , Option "o" ["output"]
            (ReqArg (addAction . writePreset) "FILE")
            "Write preset data to FILE."
          , Option "s" ["show"]
            (NoArg $ addAction showPreset)
            "Print a description of a preset."
          , Option "" ["get-name"]
            (NoArg $ addAction getName)
            "Print a preset's metadata name."
          , Option "" ["set-name"]
            (ReqArg (addAction . setName) "STRING")
            "Change a preset's metadata name."
          , Option "" ["sync-name"]
            (NoArg $ \config -> addAction (syncName config) config)
            "Change a preset's metadata name to match it's filename.\n\
            \For example, 'kpp-tool --sync-name foobar.kpp' will change\n\
            \the preset's name to \"foobar\"."
          , Option "p" ["get-param"]
            (ReqArg (addAction . getParam) "KEY")
            "Print the value of a single parameter.\n\
            \If the value is binary, it will be displayed in base-64."
          , Option "" ["set-param-string"]
            (ReqArg (addAction . setParamString) "KEY=VALUE")
            "Set the value of a string parameter."
          , Option "" ["set-param-internal"]
            (ReqArg (addAction . setParamInternal) "KEY=VALUE")
            "Set the value of an internal parameter."
          , Option "" ["set-param-binary"]
            (ReqArg (addAction . setParamBinary) "KEY=VALUE")
            "Set the value of binary (bytearray) parameter.\n\
            \VALUE should be encoded in base-64."
          , Option "e" ["extract"]
            (ReqArg (addAction . extract) "RESOURCE")
            "Extract an embedded resource."
          , Option "" ["extract-all"]
            (NoArg $ addAction extractAll)
            "Extract all embedded resources."
          , Option "" ["insert"]
            (ReqArg (addAction . insert) "RESOURCE")
            "Insert or update a resource file."
          , Option "c" ["get-icon"]
            (ReqArg (addAction . getIcon) "FILE")
            "Extract a preset's icon image."
          , Option "I" ["set-icon"]
            (ReqArg (addAction . setIcon) "FILE")
            "Change a preset's icon image.\n\
            \FILE must be a PNG file."
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

  let source = maybe BS.getContents BS.readFile configInput
      sink   = if configInPlace
               then maybe missingInputError BS.writeFile configInput
               else discard

  source
    >>= decoder
    >>= compose configActions
    >>= encoder
    >>= sink
  where
    missingInputError = error "an input file is required for --in-place"
