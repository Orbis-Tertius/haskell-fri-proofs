{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Stark.Types.CapLength (CapLength (CapLength, unCapLength)) where


import           GHC.Generics (Generic)
import Data.Kind (Type)


type CapLength :: Type
newtype CapLength = CapLength { unCapLength :: Int }
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (Num, Enum, Real, Integral)
