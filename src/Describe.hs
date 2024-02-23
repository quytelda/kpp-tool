{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Describe
  ( Describe(..)
  ) where

import           Data.List              (intersperse)
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder

-- | Build a lazy 'TL.Text' string from a list of 'Builder's
-- representing lines.
buildFromLines :: [Builder] -> TL.Text
buildFromLines = toLazyText . mconcat . intersperse "\n"

-- | Converts values to pretty textual output using lazy Text
-- builders.
class Describe a where
  -- | Generate a 'TL.Text' description of a value.
  describe :: a -> TL.Text
  describe = buildFromLines . describeLines

  -- | Generate a list of 'Builder's (representing lines) to describe
  -- a value.
  describeLines :: a -> [Builder]
  describeLines = fmap fromLazyText . TL.lines . describe
