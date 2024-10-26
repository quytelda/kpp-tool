{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App where

import           Data.Bifunctor
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as T

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
    Just ("binary",   val) -> Right $ Binary undefined
    _                      -> Left "expected TYPE:VALUE"

instance (FromArgument k, FromArgument a) => FromArgument (k, a) where
  fromArgument arg = case breakOn '=' arg of
    Just (key, val) -> (,) <$> fromArgument key <*> fromArgument val
    Nothing         -> Left "expected KEY=VALUE"

instance (Ord k, FromArgument k, FromArgument a) => FromArgument (Map.Map k a) where
  fromArgument = fmap Map.fromList . traverse fromArgument . commaSep
