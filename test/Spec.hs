{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-missing-signatures #-}
module Spec (main, spec) where

import           Prelude hiding (lookup)

import           Test.Hspec.Monadic
import           Test.Hspec.HUnit ()
import           Test.HUnit
import           Data.String.Builder (Builder, build)
import           TestUtil
import           Instance ()

import           Data.Config hiding (parse)
import qualified Data.Config as Config
import           Internal (toList)

parse :: String -> Config
parse input = case Config.parse input of
  Right x -> x
  Left err -> error err

parse_ = return . parse . build

shouldRenderTo :: Config -> Builder -> Assertion
conf `shouldRenderTo` expected = render conf `shouldBe_` expected

(-:) :: a -> (a -> b) -> b
x -: f = f x

main = hspec spec

spec = do

  describe "empty" $ do

    it "has no sections" $ do
      sections empty `shouldBe` []

    it "contains no values" $ do
      toList empty `shouldBe` []

    it "renders to empty string" $ do
      render empty `shouldBe` ""


  describe "lookup" $ do

    it "returns a value, given a section and a key" $ do
      conf <- parse_ $ do
        "[foo]"
        "bar=baz"
      lookup "foo" "bar" conf `shouldBe` Just "baz"

    it "returns Nothing, if there is no such value" $ do
      conf <- parse_ $ do
        "[foo]"
        "bar=baz"
      lookup "foo" "baz" conf `shouldBe` Nothing


  describe "insert" $ do

    it "inserts an option" $ do
      insert "foo" "bar" "baz" empty `shouldRenderTo` do
        "[foo]"
        "bar=baz"

    it "replaces an existing option" $ do
        conf <- parse_ $ do
          "[foo]"
          "bar=baz"
          "some=other"
        insert "foo" "bar" "some" conf `shouldRenderTo` do
          "[foo]"
          "bar=some"
          "some=other"

    it "removes duplicates, when replacing" $ do
        conf <- parse_ $ do
          "[foo]"
          "bar=baz"
          "bar=test"
        insert "foo" "bar" "some" conf `shouldRenderTo` do
          "[foo]"
          "bar=some"

    it "removes empty sections, when removing duplicates" $ do
        conf <- parse_ $ do
          "[foo]"
          "bar=baz"
          "[foo]"
          "bar=test"
          "[baz]"
        insert "foo" "bar" "some" conf `shouldRenderTo` do
          "[foo]"
          "bar=some"
          "[baz]"

    it "removes empty sections, when removing duplicates (at end of file)" $ do
        conf <- parse_ $ do
          "[foo]"
          "bar=baz"
          "[foo]"
          "bar=test"
        insert "foo" "bar" "some" conf `shouldRenderTo` do
          "[foo]"
          "bar=some"

    it "does not remove an empty section, when there are still comments in that section" $ do
        conf <- parse_ $ do
          "[foo]"
          "bar=baz"
          "[bar]"
          "[foo]"
          "bar=test"
          "# some comment"
        insert "foo" "bar" "some" conf `shouldRenderTo` do
          "[foo]"
          "bar=some"
          "[bar]"
          "[foo]"
          "# some comment"

    it "keeps an empty section, if it did not remove anything from that section" $ do
        conf <- parse_ $ do
          "[foo]"
          "bar=baz"
          "[foo]"
          "[bar]"
        insert "foo" "bar" "some" conf `shouldRenderTo` do
          "[foo]"
          "bar=some"
          "[foo]"
          "[bar]"

    it "keeps an empty section, if it did not remove anything from that section (at end of file)" $ do
        conf <- parse_ $ do
          "[foo]"
          "bar=baz"
          "[foo]"
        insert "foo" "bar" "some" conf `shouldRenderTo` do
          "[foo]"
          "bar=some"
          "[foo]"

  describe "delete" $ do
    it "removes an option" $ do
      parse "foo=bar" -: delete "" "foo" `shouldBe` empty

    it "removes an option from a section" $ do
      conf <- parse_ $ do
        "[foo]"
        "bar=baz"
        "key=value"
      conf -: delete "foo" "bar" `shouldRenderTo` do
        "[foo]"
        "key=value"

    it "removes an empty section, if it removed an option from that section" $ do
      conf <- parse_ $ do
        "[foo]"
        "bar=baz"
        "[bar]"
      conf -: delete "foo" "bar" `shouldRenderTo` do
        "[bar]"

    it "removes an empty section, if it removed an option from that section (at end of file)" $ do
      conf <- parse_ $ do
        "[foo]"
        "bar=baz"
      conf -: delete "foo" "bar" `shouldBe` empty

    it "does not remove an empty section, if there are still comments in that section" $ do
      conf <- parse_ $ do
        "[foo]"
        "bar=baz"
        "# some comment"
      conf -: delete "foo" "bar" `shouldRenderTo` do
        "[foo]"
        "# some comment"

    it "keeps an empty section, if it did not remove anything from that section" $ do
      conf <- parse_ $ do
        "[foo]"
        "[bar]"
      conf -: delete "foo" "bar" `shouldRenderTo` do
        "[foo]"
        "[bar]"

    it "keeps an empty section, if it did not remove anything from that section (at end of file)" $ do
      conf <- parse_ $ do
        "[foo]"
      conf -: delete "foo" "bar" `shouldRenderTo` do
        "[foo]"

  describe "sections" $ do
    it "is the sorted list of sections" $ do
      conf <- parse_ $ do
        "[a]"
        "foo=bar"
        "[b]"
        "foo=bar"
        "[c]"
        "foo=bar"
      conf -: sections `shouldBe`  ["a", "b", "c"]

    it "does not contain duplicates" $ do
      conf <- parse_ $ do
        "[a]"
        "foo=baz"
        "[a]"
        "bar=baz"
      conf -: sections `shouldBe`  ["a"]

    it "does not include empty sections" $ do
      conf <- parse_ $ do
        "[a]"
        "foo=baz"
        "[b]"
        "[c]"
      conf -: sections `shouldBe`  ["a"]
