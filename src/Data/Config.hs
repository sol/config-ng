{-# Language TemplateHaskell #-}
module Data.Config (
  Config
, Section
, Key
, Value
, empty
, parse
, render
, sections
, hasSection
, member
, lookup
, insert
, delete
) where

import           Prelude hiding (lookup)
import qualified Data.Map as Map
import qualified Data.Text as Text

import           Type
import           Internal
import           Parse (parseConfig)

parse :: String -> Either String Config
parse = parseConfig . Text.pack

render :: Config -> String
render = Text.unpack . Text.unlines . map getSource . reverse . configLines

-- | Sorted list of sections.
--
-- Duplicates and empty sections are omitted.
sections :: Config -> [Section]
sections = Map.keys . configCache

-- |
-- False for empty sections.
hasSection :: Section -> Config -> Bool
hasSection s = Map.member s . configCache
