{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, CPP #-}
module Main (main, spec) where

import           Prelude hiding (lookup)

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Hspec.Expectations.Contrib
import           Test.HUnit
import           Test.QuickCheck
import           Data.List (sort)

import           Data.Text   (Text)
import qualified Data.Text as Text

import           Data.String.Builder (Builder)
import qualified Data.String.Builder as String
import           Instance ()

import           Data.Set   (Set)
import qualified Data.Set as Set
import           Data.Foldable (foldrM)

import qualified Data.Config as Config
import           Data.Config hiding (parse)

import           Arbitrary (parse, Input(..), InputAndConfig1(..), InputAndConfig(..))

build :: Builder -> Text
build = Text.pack . String.build

parse_ :: Builder -> Config
parse_ = parse . build

shouldRenderTo :: Config -> Builder -> Assertion
conf `shouldRenderTo` expected = render conf `shouldBe` build expected

shouldBeSet :: (Show a, Ord a) => [a] -> [a] -> Assertion
actual `shouldBeSet` expected = sort actual `shouldBe` sort expected

(-:) :: a -> (a -> b) -> b
x -: f = f x

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "a valid config" $ do
    it "may have tabs and spaces before and after section headers" $ do
      parse_ $ do
        " \t  [foo]  \t  "
        "bar=baz"
      -: toList `shouldBe` [("foo", "bar", "baz")]

    it "may have tabs and spaces before an option" $ do
      parse_ $ do
        "   foo=bar"
      -: toList `shouldBe` [("", "foo", "bar")]

    it "may contain comments" $ do
      parse_ $ do
        "# foobar"
        "foo=bar"
      -: toList `shouldBe` [("", "foo", "bar")]

    it "may have tabs and spaces before a comment" $ do
      parse_ $ do
        "  \t # foobar"
        "foo=bar"
      -: toList `shouldBe` [("", "foo", "bar")]

    it "may contain empty lines" $ do
      parse_ $ do
        "foo=bar"
        "   "
      -: toList `shouldBe` [("", "foo", "bar")]

  describe "empty" $ do
    it "has no sections" $ do
      empty -: sections `shouldBe` []

    it "has no values" $ do
      empty -: toList `shouldBe` []

    it "renders to empty string" $ do
      empty -: render `shouldBe` ""

  describe "parse" $ do
    it "parses the empty string to `empty`" $ do
      Config.parse "" `shouldBe` Right empty

    it "fails on duplicate option" $ do
      Config.parse . build $ do
        "[foo]"
        "foo=bar"
        "foo=baz"
      `shouldBe` Left "duplicate key \"foo\"!"

    it "fails on duplicate option" $ do
      Config.parse . build $ do
        "foo=bar"
        "foo=baz"
      `shouldBe` Left "duplicate key \"foo\"!"

    it "fails on duplicate section" $ do
      Config.parse . build $ do
        "[foo]"
        "[foo]"
      `shouldBe` Left "duplicate section \"foo\"!"

    it "gives line number on parse error" $ do
      Config.parse . build $ do
        "[foo]"
        "[bar"
        "[baz]"
#ifdef USE_ATTOPARSEC
      `shouldBe` Left "parse error on line 2!"
#else
      `shouldBe` Left "2:5:\nunexpected \"\\n\"\nexpecting \"]\""
#endif

    it "works on input with LF line endings" $ do
        parse "[foo]\na=foo\nb=bar" -: toList `shouldBe` [("foo", "a", "foo"), ("foo", "b", "bar")]

    it "works on input with CR+LF line endings" $ do
        parse "[foo]\r\na=foo\r\nb=bar" -: toList `shouldBe` [("foo", "a", "foo"), ("foo", "b", "bar")]

    it "works on input with mixed line endings" $ do
        parse "[foo]\r\na=foo\nb=bar" -: toList `shouldBe` [("foo", "a", "foo"), ("foo", "b", "bar")]

    it "fails on a CR character that is not followed by a LF" $ do
      Config.parse . build $ do
        "[foo]"
        "bar=foo\rbar"
      `shouldSatisfy` isLeft

  describe "lookup" $ do
    it "returns a value, given a section and a key" $ do
      parse_ $ do
        "[foo]"
        "bar=baz"
      -: lookup "foo" "bar" `shouldBe` Just "baz"

    it "returns Nothing, if there is no such value" $ do
      parse_ $ do
        "[foo]"
        "bar=baz"
      -: lookup "foo" "baz" `shouldBe` Nothing

    it "returns a value from the default section, if the given section is the empty string" $ do
      parse_ $ do
        "foo=bar"
      -: lookup "" "foo" `shouldBe` Just "bar"

  describe "insert" $ do
    it "inserts an option, given a section, a key, and a value" $ do
      insert "foo" "bar" "baz" empty `shouldRenderTo` do
        "[foo]"
        "bar=baz"

    it "inserts an option into the default section, if the given section is the empty string" $ do
      insert "" "bar" "baz" empty `shouldRenderTo` do
        "bar=baz"

    it "inserts an option into the default section, if the given section is the empty string" $ do
      empty -: insert "foo" "bar" "baz" -: insert "" "key" "value" `shouldRenderTo` do
        "key=value"
        "[foo]"
        "bar=baz"

    it "inserts a new option at the beginning of a section" $ do
        -- this is important, as comments at the end of a section may be
        -- commented sections...
        parse_ $ do
          "[foo]"
          "a=foo"
          "b=bar"
        -: insert "foo" "c" "baz" `shouldRenderTo` do
          "[foo]"
          "c=baz"
          "a=foo"
          "b=bar"

    it "inserts new options at the beginning of a section (multiple inserts)" $ do
        -- this is important, as comments at the end of a section may be
        -- commented sections...
        empty
        -: insert "some" "foo" "_"
        -: insert "some" "bar" "_"
        -: insert "some" "baz" "_"
        -: insert "some" "qux" "_" `shouldRenderTo` do
          "[some]"
          "qux=_"
          "baz=_"
          "bar=_"
          "foo=_"

    it "appends new sections at the end, separated with a newline" $ do
        parse_ $ do
          "[foo]"
          "[bar]"
          "[baz]"
        -: insert "a" "b" "c" `shouldRenderTo` do
          "[foo]"
          "[bar]"
          "[baz]"
          ""
          "[a]"
          "b=c"

    it " appends new sections at the end, separated with a newline (bug fix)" $ do
        empty -: insert "one" "two" "three" -: insert "a" "b" "c" `shouldRenderTo` do
          "[one]"
          "two=three"
          ""
          "[a]"
          "b=c"

    it "replaces an existing option" $ do
        parse_ $ do
          "[foo]"
          "bar=baz"
          "some=other"
        -: insert "foo" "bar" "some" `shouldRenderTo` do
          "[foo]"
          "bar=some"
          "some=other"

    prop "replaces an existing option" $ \(InputAndConfig1 _ conf) v -> do
      (s, k, _) <- elements $ toList conf
      return $ (conf -: insert s k v -: lookup s k) == Just v

    it "preserves formating, when replacing" $ do
      parse_ $ do
        " foo\t  =   bar"
      -: insert "" "foo" "baz" `shouldRenderTo` do
        " foo\t  =   baz"

    prop "preserves formating, when replacing" $ \(InputAndConfig input conf) v ->
      let l = (toList conf)
          updatedA = foldr (\(s, k, _) c -> insert s k v c) conf     l -- change every value to v
          updatedB = foldr (\(s, k, o) c -> insert s k o c) updatedA l -- restore every value
      in render updatedB == input

  describe "delete" $ do
    it "removes an option" $ do
      parse "foo=bar" -: delete "" "foo" -: render `shouldBe` ""

    it "removes an option from a section" $ do
      parse_ $ do
        "[foo]"
        "bar=baz"
        "key=value"
      -: delete "foo" "bar" `shouldRenderTo` do
        "[foo]"
        "key=value"

    it "removes an empty section, if it removed an option from that section" $ do
      parse_ $ do
        "[foo]"
        "bar=baz"
        "[bar]"
      -: delete "foo" "bar" `shouldRenderTo` do
        "[bar]"

    it "does not remove an empty section, if there are still comments in that section" $ do
      parse_ $ do
        "[foo]"
        "bar=baz"
        "# some comment"
      -: delete "foo" "bar" `shouldRenderTo` do
        "[foo]"
        "# some comment"

    it "removes an empty section, when trying to remove a non-existing option from that section" $ do
      parse_ $ do
        "[foo]"
        "[bar]"
      -: delete "foo" "bar" `shouldRenderTo` do
        "[bar]"

    prop "is the same as insert and then delete" $ \conf s k v ->
      (conf -: insert s k v -: delete s k -: render) == (conf -: delete s k -: render)

  describe "sections" $ do
    it "returns a list of sections" $ do
      parse_ $ do
        "[a]"
        "foo=bar"
        "[b]"
        "foo=bar"
        "[c]"
        "foo=bar"
      -: sections `shouldBe`  ["a", "b", "c"]

    it "includes empty sections" $ do
      parse_ $ do
        "[a]"
        "foo=baz"
        "[b]"
        "[c]"
      -: sections `shouldBe`  ["a", "b", "c"]

    it "includes the default section" $ do
      parse_ $ do
        "foo=baz"
        "[b]"
        "[c]"
      -: sections `shouldBe`  ["", "b", "c"]

    it "does not include the default section, if it is empty" $ do
      parse_ $ do
        "# foobar"
        "[b]"
        "[c]"
      -: sections `shouldBe`  ["b", "c"]

  describe "keys" $ do
    it "returns a list of keys from a given section" $ do
      parse_ $ do
        "[a]"
        "foo="
        "bar=test"
        "baz=test"
      -: keys "a" `shouldBeSet`  ["foo", "bar", "baz"]

  describe "toList" $ do
    it "returs a list of all section-key-value tuples" $ do
      parse_ $ do
        "[a]"
        "foo=bar"
        "[b]"
        "[c]"
        "foo=baz"
      -: toList `shouldBeSet` [("a", "foo", "bar"), ("c", "foo", "baz")]

    it "returs a list of all section-key-value tuples" $ do
      parse_ $ do
        "[a]"
        "foo=bar"
        "baz=bar"
      -: toList `shouldBeSet` [("a", "foo", "bar"), ("a", "baz", "bar")]

    prop "returs a list of all section-key-value tuples" $ \(set :: Set (Section, Key)) -> do
      let myFoldrM start t f = foldrM f start t
      (l_, conf) <- myFoldrM ([], empty) set $ \(s, k) (l, acc) -> do
        v <- arbitrary
        return $ ((s, k, v) : l, insert s k v acc)
      return $ (Set.fromList . toList) conf == Set.fromList l_

  describe "render" $ do
    it "renders a config" $ do
      (empty -: insert "a" "key" "value" -: insert "" "foo" "bar") `shouldRenderTo` do
        "foo=bar"
        "[a]"
        "key=value"

    it "always adds a newline to the end" $ do
        parse "[foo]\na=foo\nb=bar" -: render `shouldBe` "[foo]\na=foo\nb=bar\n"

    it "always delimits lines with NL" $ do
        parse "[foo]\r\na=foo\r\nb=bar\r\n" -: render `shouldBe` "[foo]\na=foo\nb=bar\n"

    it "always delimits lines with NL" $ do
        parse "[foo]\r\na=foo\nb=bar\n" -: render `shouldBe` "[foo]\na=foo\nb=bar\n"

    prop "is inverse to parse (apart from newline handling)" $
      \(Input conf) -> (conf -: parse -: render) == conf
