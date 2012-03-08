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
import           Data.Text (Text)

import           Internal
import           Parse (parseConfig)

parse :: Text -> Either String Config
parse = parseConfig
