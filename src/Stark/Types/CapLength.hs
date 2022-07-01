{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Stark.Types.CapLength (CapLength (CapLength, unCapLength)) where


import GHC.Generics (Generic)


newtype CapLength = CapLength { unCapLength :: Int }
  deriving (Generic, Show, Eq, Ord, Num, Enum, Real, Integral)
