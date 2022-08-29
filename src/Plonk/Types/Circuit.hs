{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}


module Plonk.Types.Circuit
  ( Vect (..)
  , Length
  , CircuitShape (..)
  , Circuit' (Circuit)
  , Circuit
  , Constraint
  , GateConstraint (..)
  , RelativeCellRef (..)
  , RelativeRowIndex (..)
  , ColIndex (..)
  , Z2 (..)
  , Domain (..)
  , DomainGenerator (..)
  , Exponent (..)
  , Challenge (..)
  , example
  , HasData(..)
  , Entry
  , FAI(..)
  , ColType (..)
  , DegreeBound
  ) where


import           Data.Functor.Compose                         (Compose (Compose))
import           Data.Functor.Const                           (Const)
import           Data.Functor.Identity                        (Identity (Identity))
import           Data.Kind                                    (Constraint, Type)
import           Data.Vinyl.TypeLevel                         (Nat (S, Z))
import           GHC.Generics                                 (Generic)
import qualified Math.Algebra.Polynomial.Multivariate.Generic as Multi
import           Math.Algebra.Polynomial.Pretty               (Pretty (pretty))
import Plonk.Types.Fin (Fin)

type Vect :: Nat -> Type -> Type
data Vect :: Nat -> Type -> Type where
  Nil :: Vect 'Z a
  (:-) :: a -> Vect n a -> Vect ('S n) a

infixr 7 :-

instance Functor (Vect m) where
  fmap _ Nil       = Nil
  fmap f (x :- xs) = f x :- fmap f xs


type Length :: [b] -> Nat
type family Length (a :: [b]) :: Nat
type instance (Length '[]) = 'Z
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
newtype GateConstraint n d a = GateConstraint
  { unGateConstraint :: Multi.Poly a (RelativeCellRef n) }


type RelativeCellRef :: NumCols -> Type
data RelativeCellRef n = RelativeCellRef RelativeRowIndex (ColIndex n)
  deriving stock (Eq, Ord, Show)

instance Pretty (RelativeCellRef n) where
  pretty = show

type ColIndex :: NumCols -> Type
newtype ColIndex n = ColIndex { unColIndex :: Fin n }
  deriving stock (Eq, Ord, Show)


type RelativeRowIndex :: Type
newtype RelativeRowIndex = RelativeRowIndex { unRelativeRowIndex :: Int }
  deriving stock (Eq, Ord, Show)


type Exponent :: Type
newtype Exponent = Exponent { unExponent :: Int }


type NumRows :: Type
type NumRows = Nat


type CircuitShape
  :: (Type -> Type)
  -> [ColType]
  -> HasData
  -> DegreeBound
  -> Type
  -> Type
data CircuitShape f ps h d a where
  CNil :: CircuitShape f '[] h d a
  (:&) :: (Compose f (Entry h j)) a -> CircuitShape f ps h d a -> CircuitShape f ('MkCol j e : ps) h d a

type HasData :: Type
data HasData where
  WithData :: HasData
  WithNoData :: HasData

type Entry :: HasData -> FAI -> (Type -> Type)
type family Entry (y :: HasData) (x :: FAI) :: Type -> Type where
  Entry _ 'Fixed    = Identity
  Entry 'WithData _ = Identity
  Entry _ _         = Const ()

type Circuit'
  :: (Type -> Type)
  -> [ColType]
  -> HasData
  -> DegreeBound
  -> Type
  -> Type
data Circuit' f ps h d a =
  Circuit
  { shape       :: CircuitShape f ps h d a
  , constraints :: [GateConstraint (Length ps) d a]
  }
  deriving stock Generic


type Circuit :: [ColType] -> HasData -> NumRows -> DegreeBound -> Type -> Type
type Circuit ps h m d a = Circuit' (Vect m) ps h d a

infixr 7 :&

type MyC :: [ColType]
type MyC = '[ 'MkCol 'Instance 'EqCon, 'MkCol 'Advice 'NEqCon, 'MkCol 'Fixed 'EqCon ]

type Z2 :: Type
data Z2 = Zero | One

example :: CircuitShape (Vect ('S ('S ('S 'Z)))) MyC 'WithData d Z2
example = Compose (Identity One  :- Identity Zero :- Identity One :- Nil)
       :& Compose (Identity Zero :- Identity One  :- Identity Zero :- Nil)
       :& Compose (Identity One  :- Identity Zero :- Identity Zero :- Nil)
       :& CNil


type Domain :: NumRows -> Type -> Type
newtype Domain d a = Domain (DomainGenerator a)


type DomainGenerator :: Type -> Type
newtype DomainGenerator a = DomainGenerator { unDomainGenerator :: a }


type Challenge :: Type -> Type
newtype Challenge a = Challenge { unChallenge :: a }
