module Parse where

import           Prelude hiding (takeWhile)
import           Data.Text   (Text)
import qualified Data.Text as Text
import           Control.Applicative

import           Data.Attoparsec.Text hiding (option)

import Type
import Internal

parseConfig :: Text -> Either String Config
parseConfig = fmap (mkConfig . reverse) . sequence . map parseLine . Text.lines

parseLine :: Text -> Either String (WithSource Line)
parseLine input = fmap (WithSource input) $ parseOnly (line <* endOfInput) input

line :: Parser Line
line =
      mkSectionLine <$> section
  <|> uncurry mkOptionLine <$> option
  <|> comment *> pure IgnoreLine
  <|> blankLine *> pure IgnoreLine
  where
    mkSectionLine = SectionLine . Section
    mkOptionLine k v = OptionLine (Key k) (Value v)

section :: Parser Text
section = char '[' *> name <* char ']'
  where
    name = takeWhile sectionClass

sectionClass :: Char -> Bool
sectionClass = inClass "-_a-zA-Z0-9"

-- | A key–value pair
option :: Parser (Text, Text)
option = (,) <$> key <*> (separator *> value)
  where
    key = takeWhile1 keyClass
    value = takeText
    separator = skipBlanks *> skip isSeparator *> skipBlanks
    isSeparator c = c == '=' || c == ':'

keyClass :: Char -> Bool
keyClass = inClass "-_a-zA-Z0-9"

comment :: Parser ()
comment = skipBlanks *> char '#' *> takeText *> pure ()

blankLine :: Parser ()
blankLine = skipBlanks *> pure ()

skipBlanks :: Parser ()
skipBlanks = skipWhile isHorizontalSpace
