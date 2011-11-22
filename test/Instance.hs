{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instance where

import Type

deriving instance Eq   Config
deriving instance Show Config

deriving instance (Eq a)   => Eq   (WithSource a)
deriving instance (Show a) => Show (WithSource a)

deriving instance Eq   Line
deriving instance Show Line

deriving instance Eq  Value
deriving instance Ord Value
