{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Parse where

import           Prelude hiding (takeWhile)
import           Control.Applicative
import           Control.Monad
import           Data.Text   (Text)
import qualified Data.Text as Text
import           Data.Attoparsec.Text hiding (option)

import           Internal

lnum :: Text -> Text -> Int
lnum full left = succ $ length $ Text.lines parsed
  where
    parsed = Text.take (Text.length full - Text.length left) full

parse_ :: Parser a -> Text -> Either String a
parse_ p input = case flip feed "" $ parse p input of
  Fail t _ _ -> Left $ "parse error on line " ++ show (lnum input t) ++ "!"
  Done _ r   -> return r
  Partial _  -> fail "this should never happen"

parseConfig :: Text -> Either String Config
parseConfig input = join $ parse_ (config <* endOfInput) input

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
  a  <- blanks
  b  <- string "["
  c  <- blanks
  dn <- sectionName
  e  <- blanks
  f  <- string "]"
  g  <- blanks
  endOfLine <|> endOfInput
  return (Section dn, Text.concat [a, b, c, dn, e, f, g])

sectionName :: Parser Text
sectionName = takeWhile1 sectionClass

sectionClass :: Char -> Bool
sectionClass = inClass "-_a-zA-Z0-9"

sectionBody :: Parser [SectionBodyLine]
sectionBody = many sectionBodyLine

data SectionBodyLine = OptionLine Key ConfigOption | CommentLine Comment | BlankLine Text

sectionBodyLine :: Parser SectionBodyLine
sectionBodyLine = uncurry OptionLine <$> option <|> CommentLine <$> comment <|> BlankLine <$> blankLine

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
keyClass = inClass "-_a-zA-Z0-9"

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
