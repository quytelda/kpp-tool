{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Monad
import           Data.Binary
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BL
import           Data.Functor
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Prettyprinter
import           Prettyprinter.Render.Text
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

import           Preset

-- | An `Action` is function that (possibly) modifies a preset in the
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

setName :: Action
setName = undefined

getParam :: String -> Action
getParam key preset = preset <$
  case lookupParam (T.pack key) preset of
    Just val -> putDoc (pretty val) *> putChar '\n'
    Nothing  -> error $ "No such parameter: " <> key

setParam :: String -> String -> Action
setParam = undefined

writeResource :: Resource -> IO ()
writeResource Resource{..} = do
  TIO.putStrLn $ "--> Writing resource data to: " <> resourceFile
  BS.writeFile (T.unpack resourceFile) resourceData

extract :: String -> Action
extract name preset = preset <$
  case lookupResource (T.pack name) preset of
    Just res -> writeResource res
    Nothing  -> error $ "No such resource: " <> name

insert :: FilePath -> Action
insert = undefined

extractAll :: Action
extractAll preset = preset <$ mapM_ writeResource (embeddedResources preset)

-- | Helper function for decoding presets
decoder :: Applicative f => BL.ByteString -> f Preset
decoder = pure . decode

-- | Helper function for encoding presets
encoder :: Applicative f => Preset -> f BL.ByteString
encoder = pure . encode

-- | Discard is a data sink which discards any input sent to it.
discard :: Applicative f => a -> f ()
discard = const $ pure ()

compose :: [Action] -> Action
compose = foldr1 (>=>)

----------------------
-- Argument Parsing --
----------------------

data Config = Config
  { configHelp    :: Bool
  , configVersion :: Bool
  , configSource  :: IO BL.ByteString
  , configSink    :: BL.ByteString -> IO ()
  , configActions :: [Action]
  }

addAction :: Action -> Config -> Config
addAction action config@Config{..} = config { configActions = action : configActions }

addFileSink :: FilePath -> Config -> Config
addFileSink path config = config {configSink = BL.writeFile path}

options :: [OptDescr (Config -> Config)]
options = [ Option "h" ["help"]
            (NoArg $ \c -> c {configHelp    = True})
            "Display help and usage information."
          , Option "v" ["version"]
            (NoArg $ \c -> c {configVersion = True})
            "Display version information."
          , Option "o" ["output"]
            (ReqArg addFileSink "FILE")
            "Write preset data to FILE"

          -- Actions
          , Option "s" ["show"]
            (NoArg $ addAction showPreset)
            "Print preset information"
          , Option ""  ["get-name"]
            (NoArg $ addAction getName)
            "Get preset name"
          , Option "p" ["get-param"]
            (ReqArg (addAction . getParam) "KEY")
            "Get the value of a parameter"
          , Option "e" ["extract"]
            (ReqArg (addAction . extract) "NAME")
            "Extract an embedded resource"
          , Option ""  ["extract-all"]
            (NoArg $ addAction extractAll)
            "Extract all embedded resources"
          ]

defaults :: Config
defaults = Config
  { configHelp    = False
  , configVersion = False
  , configSource  = BL.getContents
  , configSink    = discard
  , configActions = []
  }

main :: IO ()
main = do
  (actions, args, errs) <- getOpt RequireOrder options <$> getArgs

  unless (null errs) $ do
    hPutStr stderr $ unlines errs
    exitFailure

  let setInput []       c = c
      setInput (path:_) c = c { configSource = BL.readFile path }
      Config{..} = foldl (.) (setInput args) actions defaults

  when configHelp $ do
    putStrLn $ usageInfo "kpp-tool" options
    exitSuccess

  when configVersion $ do
    putStrLn "not implemented yet"
    exitSuccess

  configSource
    >>= decoder
    >>= compose configActions
    >>= encoder
    >>= configSink
