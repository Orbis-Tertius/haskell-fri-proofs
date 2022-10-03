module Stark.Types.Index ( Index (Index, unIndex) ) where


import           Data.Bits (Bits)
import           Data.Kind (Type)
import Data.Word (Word64)

type Index :: Type
newtype Index = Index { unIndex :: Word64 }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Num, Real, Integral, Bits)
