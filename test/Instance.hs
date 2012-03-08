{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instance where

import Internal

deriving instance Show Config
deriving instance Show ConfigSection
deriving instance Show ConfigOption
deriving instance Show Comment

deriving instance Eq Config
deriving instance Eq ConfigSection
deriving instance Eq ConfigOption
deriving instance Eq Comment

deriving instance Ord Value
