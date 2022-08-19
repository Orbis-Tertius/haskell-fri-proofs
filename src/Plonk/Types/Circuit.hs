{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}


module Plonk.Types.Circuit
  ( Vect (..)
  , Length
  , Fin (..)
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
  , ColType
  , DegreeBound  
  ) where


import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Const
import Data.Type.Natural hiding (Zero)
import qualified Data.Type.Natural as N
import Data.Kind
import GHC.Generics (Generic)
import qualified GHC.TypeLits as TL
import qualified Math.Algebra.Polynomial.Multivariate.Generic as Multi


data Vect :: Nat -> Type -> Type where
  Nil :: Vect N.Zero a
  (:-) :: a -> Vect n a -> Vect (S n) a

infixr 7 :-

instance Functor (Vect m) where
  fmap _ Nil = Nil
  fmap f (x :- xs) = f x :- fmap f xs


type family Length (a :: [b]) :: Nat
type instance (Length '[]) = 0
type instance Length (a ': as) = 1 TL.+ Length as

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


type Fin :: Nat -> Type
data Fin n where
  FZ :: Fin n
  FS :: Fin n -> Fin (S n)


type ColIndex :: NumCols -> Type
newtype ColIndex n = ColIndex { unColIndex :: Fin n }


type RelativeRowIndex :: Type
newtype RelativeRowIndex = RelativeRowIndex { unRelativeRowIndex :: Int }


type Exponent :: Type
newtype Exponent = Exponent { unExponent :: Int }


type NumRows :: Type
type NumRows = Nat


data CircuitShape
    :: (Type -> Type)
    -> [ColType]
    -> HasData
    -> DegreeBound
    -> Type
    -> Type where
  CNil :: CircuitShape f '[] h d a
  (:&) :: (Compose f (Entry h j)) a -> CircuitShape f ps h d a -> CircuitShape f (('MkCol j e) : ps) h d a

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
  { shape :: CircuitShape f ps h d a
  , constraints :: [GateConstraint (Length ps) d a]
  }
  deriving Generic


type Circuit :: [ColType] -> HasData -> NumRows -> DegreeBound -> Type -> Type
type Circuit ps h m d a = Circuit' (Vect m) ps h d a

infixr 7 :&

type MyC :: [ColType]
type MyC = '[ 'MkCol 'Instance 'EqCon, 'MkCol 'Advice 'NEqCon, 'MkCol 'Fixed 'EqCon ]

data Z2 = Zero | One

example :: CircuitShape (Vect 3) MyC 'WithData d Z2
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
