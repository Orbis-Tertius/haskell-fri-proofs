module Stark.Types.Index (Index (Index, unIndex)) where

import Codec.Serialise (Serialise)
import Data.Bits (Bits)
import Data.Kind (Type)
import Data.Word (Word64)
import GHC.Generics (Generic)

type Index :: Type
newtype Index = Index {unIndex :: Word64}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Enum, Num, Real, Integral, Bits, Serialise)
