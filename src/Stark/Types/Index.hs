module Stark.Types.Index ( Index (Index, unIndex) ) where


import           Data.Bits (Bits)


newtype Index = Index { unIndex :: Int }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Num, Real, Integral, Bits)
