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
import qualified Data.Map as Map

import           Internal
import           Parse (parseConfig)

parse :: String -> Either String Config
parse = parseConfig . Text.pack

hasSection :: Section -> Config -> Bool
hasSection s = Map.member s . configSections
