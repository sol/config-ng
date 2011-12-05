{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Parse where

import           Prelude hiding (takeWhile)
import           Control.Applicative
import           Control.Monad
import           Data.Text   (Text)
import qualified Data.Text as Text

import           ParseUtil
import           Internal

parseConfig :: Text -> Either String Config
parseConfig input = join $ parse (config <* endOfInput) input

config :: Parser (Either String Config)
config = do
  mx <- defaultSection
  xs <- many section
  return $ join $ mkConfig `fmap` sequence (maybe xs (:xs) mx)

-- | A section header like [name], followed by options and comments
section :: Parser (Either String (Section, ConfigSection))
section = do
  (name, renderedName) <- sectionHeader
  l <- sectionBody
  return $ (name,) `fmap` mkSection_ renderedName l

defaultSection :: Parser (Maybe (Either String (Section, ConfigSection)))
defaultSection = do
  l <- sectionBody
  case l of
    [] -> return Nothing
    _  -> return $ Just $ (defaultSectionName,) `fmap` mkSection_ (unSection defaultSectionName) l

mkSection_ :: Text -> [SectionBodyLine] -> Either String ConfigSection
mkSection_ renderedName l = mkSection options comments blankLines renderedName
  where
    (options, comments, blankLines) = foldr go ([], [], []) $ zip l [0..]
    go (OptionLine key desc, i)  (as, bs, cs) = ((key, (i, desc)) : as ,         bs,          cs)
    go (CommentLine x,       i)  (as, bs, cs) = (                   as, (i, x) : bs,          cs)
    go (BlankLine x,         i)  (as, bs, cs) = (                   as,          bs, (i, x) : cs)

sectionHeader :: Parser (Section, Text)
sectionHeader = do
  pre  <- blanks
  name <- char '[' *> sectionName <* char ']'
  post  <- blanks
  endOfLine <|> endOfInput
  return (Section name, Text.concat [pre, "[", name, "]", post])

sectionName :: Parser Text
sectionName = takeWhile1 sectionClass

sectionClass :: Char -> Bool
sectionClass c = (not . elem c) "\n\r[]"

sectionBody :: Parser [SectionBodyLine]
sectionBody = many sectionBodyLine

data SectionBodyLine = OptionLine Key ConfigOption | CommentLine Comment | BlankLine Text

sectionBodyLine :: Parser SectionBodyLine
sectionBodyLine = try (uncurry OptionLine <$> option) <|> try (CommentLine <$> comment) <|> try (BlankLine <$> blankLine)

-- | A key–value pair
option :: Parser (Key, ConfigOption)
option = mk <$> blanks <*> key <*> blanks <*> separator <*> blanks <*> takeLine
  where
    key = takeWhile1 keyClass
    separator = string "=" <|> string ":"

    mk pre_key _key pre_sep sep post_sep _value = (Key _key, ConfigOption renderedKey (Value _value))
      where
        renderedKey = Text.concat [pre_key, _key, pre_sep, sep, post_sep]

keyClass :: Char -> Bool
keyClass c = (not . elem c) "\n\r\t =:[]#"

comment :: Parser Comment
comment = mk <$> blanks <*> string "#" <*> takeLine
  where
    mk a b c = Comment $ Text.concat [a, b, c]

blankLine :: Parser Text
blankLine =
  -- two rules are needed, otherwise the parser would always succeed
      blanks  <* endOfLine
  <|> blanks1 <* endOfInput

blanks :: Parser Text
blanks = takeWhile isHorizontalSpace

blanks1 :: Parser Text
blanks1 = takeWhile1 isHorizontalSpace

takeLine :: Parser Text
takeLine = takeWhile (not . isEndOfLine) <* (endOfLine <|> endOfInput)
