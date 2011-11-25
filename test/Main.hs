module Main (main) where

import           Test.Hspec.Monadic

import qualified Spec
import qualified ParseSpec

run label spec = do
  putStr $ '\n' : label
  hspec $ describe label spec

main = do
  run "Spec" Spec.spec
  run "ParseSpec" ParseSpec.spec
