{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import System.Environment

import App

main :: IO ()
main = getArgs >>= start
