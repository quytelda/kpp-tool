{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Data.Functor
import           Prettyprinter
import           Prettyprinter.Render.Text

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

-- | Discard is a data sink which discards any input sent to it.
discard :: Applicative f => a -> f ()
discard = const $ pure ()

showPreset :: Action
showPreset preset = putDoc (pretty preset) *> putChar '\n' $> preset

main :: IO ()
main = putStrLn "Not implemented yet."
