{-# LANGUAGE OverloadedStrings #-}
module Internal (

  empty
, member
, lookup
, insert
, delete

  -- used by Parse
, mkConfig

  -- used by test
, toList
) where

import           Prelude hiding (lines, lookup)
import           Data.Maybe (isJust)

import qualified Data.Text as Text

import qualified Data.Map as Map

import           Type

mkConfig :: [WithSource Line] -> Config
mkConfig lines = Config lines cache
  where
    cache = foldLines ignore onOption ignore Map.empty lines
    onOption s k v _ acc = cacheInsert s k v acc
    ignore   _     _ acc = acc

cacheInsert :: Section -> Key -> Value -> Cache -> Cache
cacheInsert s k v cache = Map.alter f s cache
  where
    f Nothing  = Just $ Map.singleton k v
    f (Just m) = Just $ Map.insert    k v m

-- used by test
toList :: Config -> [(Section, Key, Value)]
toList = concat . Map.elems . Map.mapWithKey f . configCache
  where
    f s = Map.elems . Map.mapWithKey (\k v -> (s, k, v))

empty :: Config
empty = mkConfig []

member :: Section -> Key -> Config -> Bool
member s k = isJust . lookup s k

lookup :: Section -> Key -> Config -> Maybe Value
lookup s k conf = Map.lookup s (configCache conf) >>= Map.lookup k

delete :: Section -> Key -> Config -> Config
delete s k = mkConfig . delete_ s k . configLines

data DroppedPrevious = Dropped | Kept

delete_ :: Section -> Key -> [WithSource Line] -> [WithSource Line]
delete_ section key lines =
  case foldLines onSection onOption onIgnore start lines of
    (Dropped, l) -> dropHeadIfSection l
    (Kept,    l) -> l
  where
    start =
      ( Kept -- have we dropped the previous element?
      , []
      )
    onIgnore  _     x (_,       xs) = (Kept, x : xs)
    onSection _     x (Kept,    xs) = (Kept, x : xs)
    onSection _     x (Dropped, xs) = (Kept, x : dropHeadIfSection xs)
    onOption  s k _ x (_,       xs) = if section == s && key == k then (Dropped, xs) else (Kept, x : xs)

insert :: Section -> Key -> Value -> Config -> Config
insert s k v conf = (mkConfig . op s k v . configLines) conf
  where
    op = if member s k conf then replace_ else insert_

insert_ :: Section -> Key -> Value -> [WithSource Line] -> [WithSource Line]
insert_ section key value lines =
  case foldLines onSection onOption onIgnore start lines of
    (True,  l) -> l
    (False, l) -> new : mkSectionLine section : l
  where
    new = mkOptionLine key value
    start = ( False -- have we already inserted?
            , [])
    onSection s     x (b@False, xs) = if s == section then (True, new : x : xs) else (b, x : xs)
    onSection _     x (b@True,  xs) = (b, x : xs)
    onOption  _ _ _ x (b,       xs) = (b, x : xs)
    onIgnore _      x (b,       xs) = (b, x : xs)

replace_ :: Section -> Key -> Value -> [WithSource Line] -> [WithSource Line]
replace_ section key value lines =
  case foldLines onSection onOption onIgnore start lines of
    (_, Dropped, l) -> dropHeadIfSection l
    (_, Kept,    l) -> l
  where
    new = mkOptionLine key value
    start = ( False -- have we already replaced?
            , Kept  -- have we dropped the previous element? replacing qualifies as Kept!
            , [])
    onSection _    x (y, Dropped, xs) = (y, Kept, x : dropHeadIfSection xs)
    onSection _    x (y, Kept,    xs) = (y, Kept, x : xs)
    onIgnore  _    x (y, _,       xs) = (y, Kept, x : xs)
    onOption s k _ x (b@False, _, xs) = if s == section && k == key then (True, Kept, new : xs) else (b, Kept, x : xs)
    onOption s k _ x (b@True,  _, xs) = if s == section && k == key then (b,    Dropped,    xs) else (b, Kept, x : xs)

dropHeadIfSection :: [WithSource Line] -> [WithSource Line]
dropHeadIfSection lines = case lines of
  (WithSource _ (SectionLine _)) : xs -> xs
  _ -> lines

mkSectionLine :: Section -> WithSource Line
mkSectionLine section = WithSource source $ SectionLine section
  where
    source = Text.concat ["[", unSection section, "]"]

mkOptionLine :: Key -> Value -> WithSource Line
mkOptionLine key value = WithSource source $ OptionLine key value
  where
    source = Text.concat [unKey key, "=", unValue value]

foldLines
  :: (Section ->                 WithSource Line -> acc -> acc)
  -> (Section -> Key -> Value -> WithSource Line -> acc -> acc)
  -> (Section ->                 WithSource Line -> acc -> acc)
  -> acc
  -> [WithSource Line]
  -> acc
foldLines onSection onOption onIgnore start = snd . foldr step (Section "", start)
  where
    step x@(WithSource _ (SectionLine s))  (_, xs) = (s, onSection s     x xs)
    step x@(WithSource _ (OptionLine k v)) (s, xs) = (s, onOption  s k v x xs)
    step x@(WithSource _ IgnoreLine)       (s, xs) = (s, onIgnore  s     x xs)
