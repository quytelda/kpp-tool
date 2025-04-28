{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : Kpp.App
Description : Application Logic
Copyright   : (c) Quytelda Kahja, 2024
License     : BSD-3-Clause

This module contains logic related to the command-line interface,
including argument parsing, runtime configuration, and version/help
information.
-}
module Kpp.App
  ( start
  ) where

-- | `start` is the primary entrypoint of the application, intended to
-- be called by @main@. It expects a list of command line arguments.
start :: [String] -> IO ()
start args = undefined
