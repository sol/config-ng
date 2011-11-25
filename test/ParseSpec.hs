{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module ParseSpec (main, spec) where

import           Test.Hspec.Monadic
import           Test.Hspec.HUnit ()
import           TestUtil

import           Control.Applicative
import           Data.Attoparsec.Text hiding (parse, option)
import           Data.Text (Text)

import           Parse

parse :: Parser a -> Text -> a
parse parser input = case parseOnly (parser <* endOfInput) input of
  Left err -> error err
  Right a -> a

main = hspec spec

spec = do

  describe "option" $ do
    it "is a key-value pair, separate by an equals sign" $ do
      parse option "foo=bar" `shouldBe` ("foo", "bar")

    it "may have blanks and tabs before and after the equals sign" $ do
      parse option "foo \t  =  \tbar" `shouldBe` ("foo", "bar")

  describe "section" $ do
    it "is surrounded by brackets" $ do
      parse section "[foo]" `shouldBe` "foo"
