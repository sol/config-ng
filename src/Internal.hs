{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Internal (
  Config (..)
, Section (..)
, ConfigOption (..)
, Value (..)
, Key (..)
, ConfigSection (..)
, Comment (..)
, defaultSectionName
, insert
, delete
, toList
, render
, empty
, sections
, hasSection
, keys
, lookup
, ToString (..)

  -- used by Parse
, mkConfig
, mkSection
) where

import           Prelude hiding (foldr, lookup)

import           Data.String
import           Data.Text   (Text)
import qualified Data.Text as Text
import           Data.Map   (Map)
import qualified Data.Map as Map
import           Control.Applicative hiding (empty)
import           Control.Monad
import qualified Data.List as List

defaultSectionName :: Section
defaultSectionName = ""

newtype Section = Section Text
  deriving (Eq, Ord, IsString)

newtype Key = Key Text
  deriving (Eq, Ord, IsString)

newtype Value = Value Text
  deriving (Eq, IsString)

instance Show Section where
  showsPrec p = showsPrec p . unSection

instance Show Key where
  showsPrec p = showsPrec p . unKey

instance Show Value where
  showsPrec p = showsPrec p . unValue

unSection :: Section -> Text
unSection (Section s) = s

unKey :: Key -> Text
unKey (Key k) = k

unValue :: Value -> Text
unValue (Value v) = v

class ToString a where

  toString :: a -> String
  toString = Text.unpack . toText

  toText :: a -> Text
  toText = Text.pack . toString

instance ToString Section where
  toText = unSection

instance ToString Key where
  toText = unKey

instance ToString Value where
  toText = unValue

newtype Index = Index Int
  deriving (Eq, Show, Enum, Num, Ord)

newtype Comment = Comment {unComment :: Text}

empty :: Config
empty = Config Map.empty 0

sections :: Config -> [Section]
sections (Config conf _) =
  -- exclude default section, if it only contains blank lines or comments
  case mDefaultSectionSize of
    Just 0 -> filter (/= defaultSectionName) sects
    _ -> sects
  where
    sects = Map.keys conf
    mDefaultSectionSize = (Map.size . sectionOptions . snd) `fmap` Map.lookup defaultSectionName conf

hasSection :: Section -> Config -> Bool
hasSection s = Map.member s . configSections

keys :: Section -> Config -> [Key]
keys s = maybe [] (Map.keys . sectionOptions . snd) . Map.lookup s . configSections

lookup :: Section -> Key -> Config -> Maybe Value
lookup s k conf = Map.lookup s (configSections conf) >>= lookupSection k . snd

lookupSection :: Key -> ConfigSection -> Maybe Value
lookupSection k section =
  (optionValue . snd) `fmap` (Map.lookup k $ sectionOptions section)

toList :: Config -> [(Section, Key, Value)]
toList = fold (\s k v acc -> (s, k, v) : acc) []

toMap :: Config -> Map Section (Map Key Value)
toMap = fold insert_ Map.empty
  where
    insert_ s k v acc = Map.alter f s acc
      where
        f Nothing  = Just $ Map.singleton k v
        f (Just m) = Just $ Map.insert k v m

fold :: (Section -> Key -> Value -> b -> b) -> b -> Config -> b
fold f start c = Map.foldrWithKey g start $ configSections c
  where
    g s = foldSection (f s)

foldSection :: (Key -> Value -> b -> b) -> (Index, ConfigSection) -> b -> b
foldSection f (_, section) start = Map.foldrWithKey g start $ sectionOptions section
  where
    g k (_, a) acc = f k (optionValue a) acc


data Config = Config {
  configSections       :: Map Section (Index, ConfigSection)
, configNextIndex      :: Index
}

data ConfigSection = ConfigSection {
  sectionOptions      :: Map Key (Index, ConfigOption)
, sectionComments     :: [(Index, Comment)]
, sectionEmptyLines   :: [(Index, Text)]
, sectionRenderedName :: Text
, sectionMinIndex     :: Index
}

data ConfigOption = ConfigOption {
  optionRenderedKey :: Text  -- includes separator
, optionValue       :: Value -- includes horizontal spaces at the end
}

mkOption :: Key -> Value -> ConfigOption
mkOption k v = ConfigOption (unKey k `Text.append` "=") v

insertIntoSection :: Key -> Value -> ConfigSection -> ConfigSection
insertIntoSection k v s = s { sectionOptions = Map.alter alterOption k (sectionOptions s), sectionMinIndex = nextIndex }
  where
    -- We add new options at the beginning.  This is important, as comments at
    -- the end of a section may be commented sections!
    nextIndex = pred $ sectionMinIndex s
    alterOption Nothing       = Just (nextIndex, mkOption k v)
    alterOption (Just (i, x)) = Just (i, x {optionValue = v})

insert :: Section -> Key -> Value -> Config -> Config
insert s k v (Config sects nextIndex) = Config newSections (succ nextIndex)
  where
    newSections = Map.alter f s $ sects
      where
        isDefaultSection = s == defaultSectionName
        prependNewline = not (isDefaultSection || Map.null sects)
        sectionIndex = if isDefaultSection then (-1) else nextIndex
        f Nothing       = Just $ (sectionIndex, newSection)
        f (Just (i, x)) = Just $ (i, insertIntoSection k v x)

        newSection = ConfigSection {
            sectionOptions      = Map.singleton k (0, option)
          , sectionComments     = []
          , sectionEmptyLines   = []
          , sectionRenderedName = renderedName
          , sectionMinIndex     = 0
          }
          where
            option =  ConfigOption (unKey k `Text.append` "=") v
            renderedName = if s == defaultSectionName then "" else Text.concat [if prependNewline then "\n[" else "[", unSection s, "]"]

delete :: Section -> Key -> Config -> Config
delete s k c = c {configSections = Map.alter f s sects}
  where
    sects = configSections c
    f Nothing  = Nothing
    f (Just x) = deleteFromSection k x


deleteFromSection :: Key -> (Index, ConfigSection) -> Maybe (Index, ConfigSection)
deleteFromSection k (i, s)
  | isEmpty   = Nothing
  | otherwise = Just (i, s {sectionOptions = newOptions})
  where
    options    = sectionOptions s
    newOptions = Map.delete k options
    isEmpty    = Map.null newOptions && null (sectionComments s)

renderOption :: ConfigOption -> Text
renderOption (ConfigOption k v) = k `Text.append` unValue v

renderSectionBody :: ConfigSection -> [Text]
renderSectionBody s = map snd . sortByIndex $ options ++ comments ++ sectionEmptyLines s
  where
    options  = map (fmap renderOption) $ Map.elems $ sectionOptions s
    comments = map (fmap unComment) $ sectionComments s

renderSection :: ConfigSection -> [Text]
renderSection s
  | sectionRenderedName s == unSection defaultSectionName = renderSectionBody s
  | otherwise = sectionRenderedName s : renderSectionBody s

renderConfig :: Config -> [Text]
renderConfig c =
  concatMap (renderSection . snd) . sortByIndex $ Map.elems $ configSections c

render :: Config -> Text
render = Text.unlines . renderConfig

sortByIndex :: [(Index, a)] -> [(Index, a)]
sortByIndex = List.sortBy (\(a, _) (b, _) -> a `compare` b)

mkConfig :: [(Section, ConfigSection)] -> Either String Config
mkConfig l = Config <$> foldM go Map.empty (zip l [0..]) <*> return (Index $ length l)
  where
    go acc ((s, x), i) = case Map.insertLookupWithKey undefined s (i, x) acc of
      (Nothing, m) -> return m
      _ -> Left ("duplicate section " ++ show s ++ "!")

mkSection :: [(Key, (Index, ConfigOption))] -> [(Index, Comment)] -> [(Index, Text)] -> Text -> Either String ConfigSection
mkSection opts c b n = do
  o <- foldM go Map.empty opts
  return $ ConfigSection o c b n 0
  where
    go acc (k, v) = case Map.insertLookupWithKey undefined k v acc of
      (Nothing, m) -> return m
      _ -> Left ("duplicate key " ++ show k ++ "!")
