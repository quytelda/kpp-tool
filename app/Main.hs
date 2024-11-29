{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import System.Environment

import Kpp.App

main :: IO ()
main = getArgs >>= start
