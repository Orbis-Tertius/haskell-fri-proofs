module Plonk.Types.Fin
  ( Fin (..)
  , finInj
  ) where


import           Data.Kind            (Type)
import           Data.Vinyl.TypeLevel (Nat (S))

type Fin :: Nat -> Type
data Fin n where
  FZ :: Fin n
  FS :: Fin n -> Fin ('S n)

deriving stock instance Show (Fin n)

instance Eq (Fin n) where
  FZ == FZ     = True
  FS n == FS m = finInj n == finInj m
  _ == _       = False

instance Ord (Fin n) where
  FZ <= _      = True
  FS _ <= FZ   = False
  FS n <= FS m = finInj n <= finInj m

finInj :: Fin n -> Fin ('S n)
finInj FZ     = FZ
finInj (FS n) = FS (finInj n)
