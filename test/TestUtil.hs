module TestUtil where

import Data.List (sort)
import Control.Monad

import Test.HUnit
import Test.Hspec.Monadic
import Test.Hspec.Core (Example)

import Data.String.Builder

-- TODO/IDEAS
--  * Allow assertFailure in pure code
--  * it would be nice if something like
--    `[] `shouldBe` null` would work..
--
--  * ignore does not work on prop.

isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False

shouldSatisfy :: (Show a) => a -> (a -> Bool) -> Assertion
x `shouldSatisfy` p = unless (p x) (assertFailure message)
  where
    message = show x ++ " did not satisfy predicate"

shouldBe :: (Show a, Eq a) => a -> a -> Assertion
actual `shouldBe` expected = unless (actual == expected) (assertFailure message)
  where
    message = show actual ++ " was not equal to " ++ show expected

shouldBe_ :: String -> Builder -> Assertion
actual `shouldBe_` expected = actual `shouldBe` build expected

shouldBeSet :: (Show a, Ord a) => [a] -> [a] -> Assertion
actual `shouldBeSet` expected = sort actual `shouldBe` sort expected

ignore :: a -> Result
ignore _ = pending "IGNORED"

it_ :: Example v => v -> Specs
it_ = it "FIXME: add description"
