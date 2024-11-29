module Main (main) where

import           Common
import qualified Spec
import           Test.Hspec

main :: IO ()
main = withTestDir $ hspec Spec.spec
