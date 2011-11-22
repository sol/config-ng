{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Type where

import           Data.String
import           Data.Text (Text)
import           Data.Map (Map)

newtype Section = Section {unSection :: Text}
  deriving (Ord, Eq, IsString)

newtype Key = Key {unKey :: Text}
  deriving (Ord, Eq, IsString)

newtype Value = Value {unValue :: Text}
  deriving IsString

data WithSource a = WithSource {
  getSource :: Text
, rmSource :: a
}

instance Show Section where
  showsPrec p = showsPrec p . unSection

instance Show Key where
  showsPrec p = showsPrec p . unKey

instance Show Value where
  showsPrec p = showsPrec p . unValue

data Line =
    SectionLine Section
  | OptionLine Key Value
  | IgnoreLine

data Config = Config {
  configLines :: [WithSource Line]
, configCache :: Cache
}

type Cache = Map Section (Map Key Value)
