module Data.Config (
  Config
, Section
, Key
, Value
, empty
, parse
, render
, lookup
, insert
, delete
, sections
, keys
, hasSection
, toList
, ToString(..)
) where

import           Prelude hiding (lookup)
import qualified Data.Text as Text

import           Internal
import           Parse (parseConfig)

parse :: String -> Either String Config
parse = parseConfig . Text.pack
