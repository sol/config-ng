module TestUtil where

import Data.List (sort)
import Control.Monad

import Test.HUnit
import Test.Hspec.Core (Example)
import Test.Hspec.ShouldBe

import Data.String.Builder

-- TODO/IDEAS
--  * Allow assertFailure in pure code
--  * it would be nice if something like
--    `[] `shouldBe` null` would work..
--
--  * ignore does not work on prop.

shouldBe_ :: String -> Builder -> Assertion
actual `shouldBe_` expected = actual `shouldBe` build expected

shouldBeSet :: (Show a, Ord a) => [a] -> [a] -> Assertion
actual `shouldBeSet` expected = sort actual `shouldBe` sort expected

ignore :: a -> Result
ignore _ = pending "IGNORED"

it_ :: Example v => v -> Specs
it_ = it "FIXME: add description"
