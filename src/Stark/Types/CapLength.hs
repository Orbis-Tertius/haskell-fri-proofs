module Stark.Types.CapLength (CapLength (CapLength, unCapLength)) where


import           Data.Kind    (Type)
import           GHC.Generics (Generic)


type CapLength :: Type
newtype CapLength = CapLength { unCapLength :: Int }
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (Num, Enum, Real, Integral)
