module Stark.Types.Index ( Index (Index, unIndex) ) where


import           Data.Bits (Bits)
import Data.Kind (Type)

type Index :: Type
newtype Index = Index { unIndex :: Int }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Num, Real, Integral, Bits)
