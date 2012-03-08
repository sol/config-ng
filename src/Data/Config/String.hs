module Data.Config.String (
  Config
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
) where

import           Prelude hiding (lookup)
import           Data.String

import           Data.Config (Config, empty, render, toString)
import qualified Data.Config as Plain

import qualified Data.Text as Text

parse :: String -> Either String Config
parse = Plain.parse . Text.pack

lookup :: String -> String -> Config -> Maybe String
lookup s k c = toString `fmap` Plain.lookup (fromString s) (fromString k) c

insert :: String -> String -> String -> Config -> Config
insert s k v c = Plain.insert (fromString s) (fromString k) (fromString v) c

delete :: String -> String -> Config -> Config
delete s k c = Plain.delete (fromString s) (fromString k) c

sections :: Config -> [String]
sections = map toString . Plain.sections

keys :: String -> Config -> [String]
keys s = map toString . Plain.keys (fromString s)

hasSection :: String -> Config -> Bool
hasSection s = Plain.hasSection (fromString s)

toList :: Config -> [(String, String, String)]
toList = map (\(s, k, v) -> (toString s, toString k, toString v)) . Plain.toList
