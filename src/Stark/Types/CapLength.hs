module Stark.Types.CapLength (CapLength (CapLength, unCapLength)) where


import           Data.Kind    (Type)
import           Data.Word    (Word64)
import           GHC.Generics (Generic)


type CapLength :: Type
newtype CapLength = CapLength { unCapLength :: Word64 }
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (Num, Enum, Real, Integral)
