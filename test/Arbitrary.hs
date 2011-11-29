{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Arbitrary where

import           Control.Applicative hiding (empty)
import           Data.Text   (Text)
import qualified Data.Text as Text
import           Data.String
import           Test.QuickCheck
import           Data.Attoparsec.Text (isEndOfLine)
import           Data.Set   (Set)
import qualified Data.Set as Set
import           Control.Monad (foldM)

import           Instance ()
import qualified Parse
import           Internal
import qualified Data.Config as Config

parse :: String -> Config
parse input = case Config.parse input of
  Right x -> x
  Left err -> error err

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = Set.fromList <$> arbitrary

instance Arbitrary Section where
  arbitrary = Section <$> sectionName

instance Arbitrary Key where
  arbitrary = Key <$> key

instance Arbitrary Value where
  arbitrary = Value <$> value

instance Arbitrary Config where
  arbitrary = parse <$> config

concatG :: [Gen Text] -> Gen Text
concatG l = Text.concat <$> sequence l

blanks :: Gen Text
blanks  = resize 5 $ fromString <$> (listOf . elements) [' ', '\t']

arbitraryLine :: Gen Text
arbitraryLine = resize 10 $ fromString <$> listOf (arbitrary `suchThat` (not . isEndOfLine))

sectionName :: Gen Text
sectionName = resize 5 $ fromString <$> listOf (arbitrary `suchThat` Parse.sectionClass)

key :: Gen Text
key = resize 5 $ fromString <$> listOf1 (arbitrary `suchThat` Parse.keyClass)

value :: Gen Text
value = resize 10 $ Text.stripStart <$> arbitraryLine

sectionHeader :: Text -> Gen Text
sectionHeader name = concatG [blanks, pure "[", blanks, pure name, blanks, pure "]", blanks]

option :: Text -> Gen Text
option k = concatG [blanks, pure k, blanks, separator, blanks, value]
  where
    separator = elements ["=", ":"]

comment :: Gen Text
comment = concatG [blanks, pure "#", arbitraryLine]

blankLine :: Gen Text
blankLine = blanks

data Line = OptionLine | CommentLine | BlankLine
  deriving Show

sectionBody :: Gen [Text]
sectionBody = do
  l <- listOf $ frequency [(2, pure OptionLine), (1, pure CommentLine), (1, pure BlankLine)]
  snd `fmap` foldM f (Set.empty, []) l
  where
    f (set, xs) OptionLine = do
      k <- key `suchThat` flip Set.notMember set
      x <- option k
      return (Set.insert k set, x : xs)
    f (set, xs) CommentLine = do
      x <- comment
      return (set, x : xs)
    f (set, xs) BlankLine = do
      x <- blankLine
      return (set, x : xs)

configBody :: Gen [[Text]]
configBody = do
  sectionNames <- resize 10 $ listOf sectionBody
  (_, l) <- foldM f (Set.empty, []) sectionNames
  defaultSection <- oneof [pure Nothing, Just <$> sectionBody]
  return $ maybe l (:l) defaultSection
  where
    f (set, xs) body = do
      name <- sectionName `suchThat` (\n -> n /= unSectionName defaultSectionName && Set.notMember n set)
      header <- sectionHeader name
      return (Set.insert name set, (header : body) : xs)

config :: Gen String
config = Text.unpack . Text.unlines . concat <$> configBody

inputAndConfig :: Gen (String, Config)
inputAndConfig = do
  input <- config
  return $ (input, parse input)

inputAndConfig1 :: Gen (String, Config)
inputAndConfig1 = inputAndConfig `suchThat` (not . null . toList . snd)

data Input = Input String
  deriving Show

instance Arbitrary Input where
  arbitrary = Input<$> config

data InputAndConfig = InputAndConfig String Config
  deriving Show

instance Arbitrary InputAndConfig where
  arbitrary = uncurry InputAndConfig <$> inputAndConfig

data InputAndConfig1 = InputAndConfig1 String Config
  deriving Show

instance Arbitrary InputAndConfig1 where
  arbitrary = uncurry InputAndConfig1 <$> inputAndConfig1

sampleConfig :: IO ()
sampleConfig = sample' config >>= mapM_ (\s -> putStrLn s >> putStrLn (replicate 78 '#'))
