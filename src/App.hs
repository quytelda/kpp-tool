{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App where

import           Control.Applicative
import           Control.Monad
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
import           System.IO

import           Preset

breakOn :: Eq a => a -> [a] -> Maybe ([a], [a])
breakOn c = go []
  where
    go xs (y:ys) | c /= y    = go (y:xs) ys
                 | otherwise = Just (reverse xs, ys)
    go _ _                   = Nothing

commaSep :: String -> [String]
commaSep [] = []
commaSep xs = uncurry (:) . second commaSep . break (== ',') $ xs

-- | FromArgument is a class for types that can be parsed from a
-- string argument to a CLI option, e.g. --some-flag=<ARG>.
class FromArgument a where
  fromArgument :: String -> Either String a

fromArgument_ :: FromArgument a => String -> a
fromArgument_ s = case fromArgument s of
  Right v  -> v
  Left err -> error err

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

data RuntimeConfig = RuntimeConfig
  { runHelp       :: Bool
  , runVersion    :: Bool
  , runInputPath  :: Maybe FilePath
  , runOverwrite  :: Bool
  , runOperations :: [Op]
  }

defaults :: RuntimeConfig
defaults = RuntimeConfig
  { runHelp       = False
  , runVersion    = False
  , runInputPath  = Nothing
  , runOverwrite  = False
  , runOperations = []
  }

addOperation :: Op -> RuntimeConfig -> RuntimeConfig
addOperation op config@RuntimeConfig{..} = config { runOperations = op : runOperations }

writeResource :: MonadIO m => Maybe FilePath -> Resource -> m ()
writeResource mpath Resource{..} = liftIO $ do
  putStrLn $ "==> Writing resource data to: " <> path
  BS.writeFile path resourceData
  where
    path = fromMaybe (T.unpack resourceFile) mpath

type Op = StateT Preset IO ()

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

op_setParam :: (Text, ParamValue) -> Op
op_setParam = modify' . uncurry insertParam

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

op_extractAll :: Op
op_extractAll = do
  resources <- gets embeddedResources
  mapM_ (writeResource Nothing) resources

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
op_output path = do
  preset <- get
  liftIO $ savePreset path preset

op_syncName :: RuntimeConfig -> Op
op_syncName RuntimeConfig{..} =
  case runInputPath of
    Just path -> op_setName (T.pack path)
    Nothing   -> error "--sync-name requires an input path"

options :: [OptDescr (RuntimeConfig -> RuntimeConfig)]
options = [ Option "h" ["help"]
            (NoArg $ \c -> c { runHelp = True })
            "Display help and usage information."
          , Option "v" ["version"]
            (NoArg $ \c -> c { runVersion = True })
            "Display version information."
          , Option "O" ["overwrite"]
            (NoArg $ \c -> c { runOverwrite = True })
            "Modify a preset file in-place."

          -- Operations
          , Option "o" ["output"]
            (ReqArg (addOperation . op_output . fromArgument_) "PATH")
            "Write preset to PATH"
          , Option "i" ["info"]
            (NoArg  (addOperation op_info))
            "Show preset info"
          , Option "n" ["get-name"]
            (NoArg  (addOperation op_getName))
            "Show preset name"
          , Option "N" ["set-name"]
            (ReqArg (addOperation . op_setName . fromArgument_) "STRING")
            "Set name"
          , Option "S" ["sync-name"]
            (NoArg $ \rc -> addOperation (op_syncName rc) rc)
            "Sync name"
          , Option "p" ["get-param"]
            (ReqArg (addOperation . op_getParam . fromArgument_) "KEY")
            "Get param"
          , Option "P" ["set-param"]
            (ReqArg (addOperation . op_setParam . fromArgument_) "KEY=TYPE:VALUE")
            "Set param"
          , Option "x" ["extract"]
            (ReqArg (addOperation . op_extract . fromArgument_) "KEY=VALUE[,...]")
            "Extract a resource"
          , Option "X" ["extract-all"]
            (NoArg  (addOperation op_extractAll))
            "Extract all resources"
          , Option "c" ["get-icon"]
            (ReqArg (addOperation . op_getIcon . fromArgument_) "PATH")
            "Extract PNG icon"
          , Option "C" ["set-icon"]
            (ReqArg (addOperation . op_setIcon . fromArgument_) "PATH")
            "Set PNG icon"
          ]
