{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : App
Description : Application Logic
Copyright   : (c) Quytelda Kahja, 2024
License     : BSD-3-Clause

This module contains logic related to the command-line interface,
including argument parsing, runtime configuration, and version/help
information.
-}
module App
  ( kppToolVersion
  , FromArgument(..)
  , fromArgument_
  , RunConfig(..)
  , RunMode(..)
  , Op
  , run
  , start
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Binary               hiding (get, put)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Map.Strict           as Map
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Data.Version
import           Prettyprinter
import           Prettyprinter.Render.Text
import           System.Console.GetOpt
import           System.Exit
import           System.FilePath
import           System.IO

import qualified Paths_kpp_tool
import           Preset

-- | The kpp-tool version, exported from the Paths module
-- automatically generated by Cabal.
kppToolVersion :: Version
kppToolVersion = Paths_kpp_tool.version

breakOn :: Eq a => a -> [a] -> Maybe ([a], [a])
breakOn c = go []
  where
    go xs (y:ys) | c /= y    = go (y:xs) ys
                 | otherwise = Just (reverse xs, ys)
    go _ _                   = Nothing

-- | Split a `Prelude.String` on the first comma which is not escaped
-- with backslash.
splitOnComma :: String -> (String, String)
splitOnComma []              = ([], [])
splitOnComma (',' : cs)      = ([], cs)
splitOnComma ('\\' : c : cs) = first (c :) $ splitOnComma cs
splitOnComma (c : cs)        = first (c :) $ splitOnComma cs

commaSep :: String -> [String]
commaSep [] = []
commaSep xs = uncurry (:) . second commaSep . splitOnComma $ xs

-- | FromArgument is a class for types that can be parsed from a
-- `Prelude.String` argument to a CLI option, e.g. @--some-flag=ARGUMENT@.
class FromArgument a where
  fromArgument :: String -> Either String a

-- | The same as `fromArgument` but calls `error` on failure.
fromArgument_ :: FromArgument a => String -> a
fromArgument_ s = case fromArgument s of
  Right v  -> v
  Left err -> error err

fromArgumentOptional :: FromArgument a => Maybe String -> Maybe a
fromArgumentOptional = fmap fromArgument_

instance FromArgument String where
  fromArgument = pure

instance FromArgument Text where
  fromArgument = pure . T.pack

instance FromArgument ParamValue where
  fromArgument arg = case breakOn ':' arg of
    Just ("string",   val) -> Right $ String (T.pack val)
    Just ("internal", val) -> Right $ Internal (T.pack val)
    Just ("binary",   val) -> Binary <$> decodeBase64 (T.pack val)
    _                      -> Left "expected TYPE:VALUE"

instance (FromArgument k, FromArgument a) => FromArgument (k, a) where
  fromArgument arg = case breakOn '=' arg of
    Just (key, val) -> (,) <$> fromArgument key <*> fromArgument val
    Nothing         -> Left "expected KEY=VALUE"

instance (Ord k, FromArgument k, FromArgument a) => FromArgument (Map.Map k a) where
  fromArgument = fmap Map.fromList . traverse fromArgument . commaSep

-- | The settings required for a normal operational run.
data RunConfig = RunConfig
  { runInputPath  :: Maybe FilePath -- ^ A KPP file to process
  , runOverwrite  :: Bool           -- ^ Whether to overwrite the input file
  , runQuiet      :: Bool           -- ^ Supress unnecessary output
  , runOperations :: [Op]           -- ^ Operations to run on the input
  }

-- | Default Runtime Options
defaults :: RunConfig
defaults = RunConfig
  { runInputPath  = Nothing
  , runOverwrite  = False
  , runQuiet      = False
  , runOperations = []
  }

-- | A sum type representing different program operating modes which
-- require different settings.
data RunMode = HelpMode          -- ^ Display help and usage information.
             | VersionMode       -- ^ Display version information.
             | RunMode RunConfig -- ^ Run a sequence of operations.

mapConfig :: (RunConfig -> RunConfig) -> RunMode -> RunMode
mapConfig f (RunMode x) = RunMode (f x)
mapConfig _ HelpMode    = HelpMode
mapConfig _ VersionMode = VersionMode

setHelpMode :: RunMode -> RunMode
setHelpMode _ = HelpMode

setVersionMode :: RunMode -> RunMode
setVersionMode HelpMode = HelpMode
setVersionMode _        = VersionMode

addOperation :: Op -> RunMode -> RunMode
addOperation op = mapConfig $ \config@RunConfig{..} ->
  config { runOperations = op : runOperations }

-- | A composable processing operation which acts on preset data.
type Op = StateT Preset (ReaderT RunConfig IO) ()

runOp :: Op -> RunConfig -> Preset -> IO Preset
runOp op = flip $ runReaderT . execStateT op

-- | Save a `Resource` to file. An optional output path can be
-- provided; otherwise, the resource's filename property is used.
writeResource :: Maybe FilePath -> Resource -> Op
writeResource mpath resource = do
  path <- liftIO $ saveResource mpath resource

  -- If no output path was specified, we inform the user where the
  -- output was written.
  RunConfig{..} <- lift ask
  unless (runQuiet || isJust mpath) $
    liftIO $ putStrLn $ "Wrote resource to: " <> path

op_info :: Op
op_info = do
  preset <- get
  liftIO $ putDoc (pretty preset)
  liftIO $ putChar '\n'

op_getName :: Op
op_getName = do
  name <- gets presetName
  liftIO $ TIO.putStrLn name

op_setName :: Text -> Op
op_setName = modify' . setPresetName

op_getParam :: Text -> Op
op_getParam key = do
  preset <- get
  case lookupParam key preset of
    Just val -> liftIO $ putDoc (pretty val) *> putChar '\n'
    Nothing  -> fail $ "no such parameter: " <> T.unpack key

op_listParams :: Op
op_listParams = do
  Preset{..} <- get
  liftIO $ putDoc (prettyParams presetParams)
  liftIO $ putChar '\n'

op_setParam :: (Text, ParamValue) -> Op
op_setParam = modify' . uncurry insertParam

op_listResources :: Op
op_listResources = do
  Preset{..} <- get
  liftIO $ putDoc (prettyResources embeddedResources)
  liftIO $ putChar '\n'

op_extract :: Map.Map Text Text -> Op
op_extract opts = do
  let mpath = T.unpack <$> Map.lookup "path" opts
      lookupResource preset =
        (Map.lookup "name" opts >>= flip lookupResourceByName preset) <|>
        (Map.lookup "file" opts >>= flip lookupResourceByFile preset) <|>
        (Map.lookup "md5"  opts >>= flip lookupResourceByMD5  preset)

  preset <- get
  case lookupResource preset of
    Just resource -> writeResource mpath resource
    Nothing       -> fail "extract: no matching resource found"

op_extractAll :: Maybe FilePath -> Op
op_extractAll mdir = do
  resources <- gets embeddedResources
  forM_ resources $ \resource@Resource{..} ->
    let mpath = do
          dir <- mdir
          pure $ dir </> takeFileName (T.unpack resourceFile)
    in writeResource mpath resource

op_embed :: Map.Map Text Text -> Op
op_embed opts = do
  preset <- get

  -- TODO: This can probably be simplified by using a second internal
  -- "do" block and constructing the Resource using RecordWildCards.
  rData <- liftIO $ traverse BS.readFile (T.unpack <$> rPath)
  case Resource <$> rName <*> rFile <*> rType <*> rData of
    Just resource -> put $ insertResource resource preset
    Nothing       -> fail "insert: invalid resource definition"
  where
    rPath = Map.lookup "path" opts <|> Map.lookup "file" opts
    rType = Map.lookup "type" opts
    rName = Map.lookup "name" opts <|> rPath
    rFile = Map.lookup "file" opts <|> rPath

op_getIcon :: FilePath -> Op
op_getIcon path = do
  icon <- gets getPresetIcon
  liftIO $ BL.writeFile path icon

op_setIcon :: FilePath -> Op
op_setIcon path = do
  pngData <- liftIO $ BL.readFile path
  preset <- get
  case setPresetIcon pngData preset of
    Right preset' -> put preset'
    Left  err     -> fail $ "set-icon: " <> err

op_output :: FilePath -> Op
op_output path = get >>= liftIO . write . encode
  where
    write = case path of
      "-" -> BL.putStr
      _   -> BL.writeFile path

op_syncName :: Op
op_syncName = do
  RunConfig{..} <- lift ask
  case runInputPath of
    Just path -> op_setName $ T.pack (takeBaseName path)
    Nothing   -> error "--sync-name requires an input path"

-- | Command Line Options
options :: [OptDescr (RunMode -> RunMode)]
options = [ Option "h" ["help"]
            (NoArg setHelpMode)
            "Display help and usage information."
          , Option "v" ["version"]
            (NoArg setVersionMode)
            "Display version information."

          -- Global Options
          , Option "O" ["overwrite"]
            (NoArg $ mapConfig $ \c -> c { runOverwrite = True })
            "Modify a preset file in-place."
          , Option "q" ["quiet"]
            (NoArg $ mapConfig $ \c -> c { runQuiet = True })
            "Supress unnecessary output."

          -- Operations
          , Option "o" ["output"]
            (ReqArg (addOperation . op_output . fromArgument_) "PATH")
            "Write preset data to PATH.\n\
            \If PATH is \"-\", data will be written to stdout."
          , Option "i" ["info"]
            (NoArg  (addOperation op_info))
            "Print a description of a preset."
          , Option "n" ["get-name"]
            (NoArg  (addOperation op_getName))
            "Print a preset's metadata name."
          , Option "N" ["set-name"]
            (ReqArg (addOperation . op_setName . fromArgument_) "STRING")
            "Change a preset's metadata name."
          , Option "S" ["sync-name"]
            (NoArg (addOperation op_syncName))
            "Change a preset's metadata name to match it's filename.\n\
            \For example, 'kpp-tool --sync-name foobar.kpp' will change\n\
            \the preset's name to \"foobar\"."
          , Option "l" ["list-params"]
            (NoArg (addOperation op_listParams))
            "Print a table of all parameters."
          , Option "p" ["get-param"]
            (ReqArg (addOperation . op_getParam . fromArgument_) "KEY")
            "Print the value of a single parameter."
          , Option "P" ["set-param"]
            (ReqArg (addOperation . op_setParam . fromArgument_) "KEY=TYPE:VALUE")
            "Set the value of a parameter.\n\
            \TYPE can be 'string', 'internal', or 'binary'.\n\
            \For binary parameters, VALUE should be encoded in base-64."
          , Option "r" ["list-resources"]
            (NoArg (addOperation op_listResources))
            "Print a table of all embedded resources."
          , Option "x" ["extract"]
            (ReqArg (addOperation . op_extract . fromArgument_) "KEY=VALUE[,...]")
            "Extract an embedded resource."
          , Option "X" ["extract-all"]
            (OptArg  (addOperation . op_extractAll . fromArgumentOptional) "DIR")
            "Extract all embedded resources.\n\
            \If the optional directory path argument is provided,\n\
            \the extracted files will be placed in DIR. In this case,\n\
            \the '=' sign syntax is required."
          , Option "e" ["embed"]
            (ReqArg (addOperation . op_embed . fromArgument_) "KEY=VALUE[,...]")
            "Insert or update a resource file."
          , Option "c" ["get-icon"]
            (ReqArg (addOperation . op_getIcon . fromArgument_) "PATH")
            "Extract a preset's PNG icon image."
          , Option "C" ["set-icon"]
            (ReqArg (addOperation . op_setIcon . fromArgument_) "PATH")
            "Change a preset's icon image.\n\
            \FILE must be a PNG file."
          ]

-- | Run the program with a given configuration.
run :: RunMode -> IO ()
run HelpMode    = putStrLn $ usageInfo "Usage: kpp-tool [OPTION]... [FILE]" options
run VersionMode = putStrLn $ "kpp-tool " <> showVersion kppToolVersion
run (RunMode config@RunConfig{..}) = do
  let source  = maybe BS.getContents BS.readFile runInputPath
      parse   = pure . decode . BL.fromStrict
      process = runOp (sequence_ runOperations) config
      render  = pure . encode
      sink    = when runOverwrite . maybe BL.putStr BL.writeFile runInputPath

  source >>= parse >>= process >>= render >>= sink

-- | `start` is the primary entrypoint of the application, intended to
-- be called by @main@. It expects a list of command line arguments.
start :: [String] -> IO ()
start args = do
  let (flags, posArgs, errs) = getOpt RequireOrder options args

  -- if parsing CLI arguments failed
  unless (null errs) $ do
    hPutStr stderr $ unlines errs
    exitFailure

  let settings = foldr (.) id flags $
        RunMode defaults { runInputPath = listToMaybe posArgs }

  run settings
