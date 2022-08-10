{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Plonk.Types.Circuit
  ( Circuit (..)
  , Constraint (..)
  , RelativeCellRef (..)
  , Polynomial (..)
  , Monomial (..)
  , Exponent (..)
  , FixedValues (..)
  , CellIndex (..)
  , RowIndex (..)
  , RelativeRowIndex (..)
  , ColIndex (..)
  ) where


import Data.Map (Map)
import Data.Type.Natural hiding (Zero)
import qualified Data.Type.Natural as N
import Data.Kind
import GHC.TypeNats

import Stark.Types.Scalar (Scalar)


type FAI :: Type
data FAI = Instance | Advice | Fixed

type EN :: Type
data EN = EqCon | NEqCon

type ColType :: Type
data ColType = MkCol FAI EN

type C :: Nat -> Type
type C n = Vect n ColType


data Vect :: Nat -> Type -> Type where
  Nil :: Vect N.Zero a
  (:-) :: a -> Vect n a -> Vect (S n) a

infixr 7 :-


type NumCols :: Type
type NumCols = Nat


type DegreeBound :: Type
type DegreeBound = Nat


type GateConstraint :: NumCols -> DegreeBound -> Type -> Type
newtype GateConstraint n d a = GateConstraint { unGateConstraint :: Polynomial a (RelativeCellRef n) d }


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


type Polynomial :: Type -> Type -> DegreeBound -> Type
newtype Polynomial a v d = Polynomial { unPolynomial :: [Monomial a v d] }


type Monomial :: Type -> Type -> DegreeBound -> Type
data Monomial a v d = Monomial a (Vect d v)


type NumRows :: Type
type NumRows = Nat


data CircuitShape :: forall (n :: Nat). Vect n ColType -> NumRows -> DegreeBound -> Type -> Type where
  CNil :: CircuitShape Nil m d a
  (:&) :: Vect m a -> CircuitShape ps m d a -> CircuitShape ((MkCol Fixed e) :- ps) m d a
  CAdvice :: CircuitShape ps m d a -> CircuitShape ((MkCol Advice e) :- ps) m d a
  CInstance :: CircuitShape ps m d a -> CircuitShape ((MkCol Instance e) :- ps) m d a


type Circuit :: forall (n :: Nat). Vect n ColType -> NumRows -> DegreeBound -> Type -> Type
data Circuit ps m d a =
  Circuit
  { shape :: CircuitShape ps m d a
  , constraints :: [GateConstraint n d a]
  }


data InstanceData :: forall (n :: Nat). Vect n ColType -> NumRows -> Type where
  INil :: InstanceData Nil m
  IConsFixed :: (y :: InstanceData v m) -> InstanceData ((MkCol Fixed e) :- v) (xs :& y)


infixr 7 :&
infixr 7 :*
infixr 7 :^
infixr 7 :+

type MyC :: C 3
type MyC = 'MkCol Instance EqCon :- 'MkCol Advice NEqCon :- 'MkCol Fixed EqCon :- Nil

data Z2 = Zero | One

f :: Circuit MyC 3 d Z2
f =  (One  :- Zero :- One :- Nil)
  :^ (Zero :- One  :- Zero :- Nil)
  :* (One  :- Zero :- Zero :- Nil)
  :& CNil
