{-# LANGUAGE OverloadedStrings, CPP #-}
-- |
-- Abstraction layer for attoparsec and Parsec.
module ParseUtil (
  Parser
, parse
, takeWhile
, takeWhile1
, endOfLine
, endOfInput
, try
, string
, isHorizontalSpace
, isEndOfLine
) where

import           Prelude hiding (takeWhile)

import           Data.Text   (Text)
import qualified Data.Text as Text

#ifdef USE_ATTOPARSEC
import           Data.Attoparsec.Text hiding (option, parse, isHorizontalSpace, isEndOfLine)
import qualified Data.Attoparsec.Text as Attoparsec
#else
import           Control.Applicative
import           Text.Parsec.Text (Parser)
import           Text.Parsec (try, char, satisfy, eof, many1)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec
#endif


#ifdef USE_ATTOPARSEC
lnum :: Text -> Text -> Int
lnum full left = succ $ length $ Text.lines parsed
  where
    parsed = Text.take (Text.length full - Text.length left) full

parse :: Parser a -> Text -> Either String a
parse p input = case flip feed "" $ Attoparsec.parse p input of
  Fail t _ _ -> Left $ "parse error on line " ++ show (lnum input t) ++ "!"
  Done _ r   -> return r
  Partial _  -> fail "this should never happen"
#else

mkError :: Parsec.ParseError -> String
mkError err = show l ++ ":" ++ show c ++ ":" ++ m
  where
    (l, c) = let pos = Parsec.errorPos err in (Parsec.sourceLine pos, Parsec.sourceColumn pos)
    m = Parsec.showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (Parsec.errorMessages err)

parse :: Parser a -> Text -> Either String a
parse p input = case Parsec.parse p "" input of
  Left err -> Left $ mkError err
  Right x -> return x

endOfLine :: Parser ()
endOfLine = Parsec.option '\r' (char '\r') >> char '\n' >> return ()

endOfInput :: Parser ()
endOfInput = eof

takeWhile :: (Char -> Bool) -> Parser Text
takeWhile p = Text.pack <$> many (satisfy p)

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 p = Text.pack <$> many1 (satisfy p)

string :: String -> Parser Text
string s = Text.pack <$> Parsec.string s
#endif

isEndOfLine :: Char -> Bool
isEndOfLine c = c == '\n' || c == '\r'

isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c == ' ' || c == '\t'
