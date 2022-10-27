{-# LANGUAGE UndecidableInstances #-}

module Plonk.Types.Circuit
  ( Length,
    CircuitShape (..),
    CircuitM (CircuitM, shape, constraints),
    Circuit,
    Constraint,
    GateConstraint (..),
    RelativeCellRef (..),
    RelativeRowIndex (..),
    ColIndex (..),
    Z2 (..),
    Domain (..),
    Exponent (..),
    Challenge (..),
    HasData (..),
    Entry,
    FAI (..),
    EN (..),
    ColType (..),
    DegreeBound,
  )
where

import Data.Functor.Compose (Compose)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.Kind (Constraint, Type)
import Data.Vinyl.TypeLevel (Nat (S, Z))
import GHC.Generics (Generic)
import qualified Math.Algebra.Polynomial.Multivariate.Generic as Multi
import Math.Algebra.Polynomial.Pretty (Pretty (pretty))
import Plonk.Types.Fin (Fin)
import Plonk.Types.Z2 (Z2 (One, Zero))

type Length :: [b] -> Nat
type family Length (a :: [b]) :: Nat

type instance Length '[] = 'Z

type instance Length (_ ': as) = 'S (Length as)

type FAI :: Type
data FAI = Instance | Advice | Fixed

type EN :: Type
data EN = EqCon | NEqCon

type ColType :: Type
data ColType = MkCol FAI EN

type NumCols :: Type
type NumCols = Nat

type DegreeBound :: Type
type DegreeBound = Nat

type GateConstraint :: NumCols -> DegreeBound -> Type -> Type
newtype GateConstraint n d a where
  MkGateConstraint :: {unGateConstraint :: Multi.Poly a (RelativeCellRef n)} -> GateConstraint n d a

type RelativeCellRef :: NumCols -> Type
data RelativeCellRef n where
  MkRelativeCellRef :: RelativeRowIndex -> ColIndex n -> RelativeCellRef n
  deriving stock (Eq, Ord, Show)

instance Pretty (RelativeCellRef n) where
  pretty = show

type ColIndex :: NumCols -> Type
newtype ColIndex n = ColIndex {unColIndex :: Fin n}
  deriving stock (Eq, Ord, Show)

type RelativeRowIndex :: Type
newtype RelativeRowIndex = RelativeRowIndex {unRelativeRowIndex :: Int}
  deriving stock (Eq, Ord, Show)

type Exponent :: Type
newtype Exponent = Exponent {unExponent :: Int}

type NumRows :: Type
type NumRows = Nat

type CircuitShape ::
  (Type -> Type) ->
  [ColType] ->
  HasData ->
  DegreeBound ->
  Type ->
  Type
data CircuitShape f ps h d a where
  CNil :: CircuitShape f '[] h d a
  (:&) :: (Compose f (Entry h j)) a -> CircuitShape f ps h d a -> CircuitShape f ('MkCol j e ': ps) h d a

type HasData :: Type
data HasData where
  WithData :: HasData
  WithNoData :: HasData

type Entry :: HasData -> FAI -> (Type -> Type)
type family Entry (y :: HasData) (x :: FAI) :: Type -> Type where
  Entry _ 'Fixed = Identity
  Entry 'WithData _ = Identity
  Entry _ _ = Const ()

type CircuitM ::
  (Type -> Type) ->
  [ColType] ->
  HasData ->
  DegreeBound ->
  Type ->
  Type
data CircuitM f ps h d a = CircuitM
  { shape :: CircuitShape f ps h d a,
    constraints :: [GateConstraint (Length ps) d a]
  }
  deriving stock (Generic)

type Circuit :: [ColType] -> HasData -> NumRows -> DegreeBound -> Type -> Type
type Circuit ps h m = CircuitM [] ps h

infixr 7 :&

type Domain :: NumRows -> Type -> Type
newtype Domain d a = Domain {fromDomain :: Int -> a}

type Challenge :: Type -> Type
newtype Challenge a = Challenge {unChallenge :: a}
