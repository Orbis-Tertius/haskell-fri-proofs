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


{--
data Circuit f =
  Circuit
  { numRows :: NumRows
  , colTypes :: ColTypes
  , degreeBound :: PolyDegreeBound
  , constraints :: Constraints
  , fixedValues :: FixedValues
  }
--}


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




data Circuit :: forall (n :: Nat). Vect n ColType -> Nat -> Type -> Type where
  CNil :: Circuit Nil n a
  (:&) :: Vect n a -> Circuit ps n a -> Circuit (p :- ps) n a

infixr 7 :&

type MyC :: C 3
type MyC = 'MkCol Instance EqCon :- 'MkCol Advice NEqCon :- 'MkCol Fixed EqCon :- Nil

data Z2 = Zero | One

f :: Circuit MyC 3 Z2
f = (One  :- Zero :- One :- Nil)
 :& (Zero :- One  :- Zero :- Nil)
 :& (One  :- Zero :- Zero :- Nil)
 :& CNil


{-
newtype NumRows = NumRows { unNumRows :: Int } 


newtype PolyDegreeBound = PolyDegreeBound { unDegreeBound :: Int }


newtype Constraints = Constraints { unConstraints :: [Constraint] }


newtype Constraint = Constraint { unConstraint :: Polynomial RelativeCellRef }


data RelativeCellRef = RelativeCellRef RelativeRowIndex ColIndex


newtype Polynomial v = Polynomial { unPolynomial :: [Monomial v] }


data Monomial v = Monomial Scalar (Map v Exponent)


newtype Exponent = Exponent { unExponent :: Int }


newtype FixedValues = FixedValues { unFixedValues :: CellIndex -> Scalar }


data CellIndex = CellIndex RowIndex


newtype RowIndex = RowIndex { unRowIndex :: Int }


newtype RelativeRowIndex = RelativeRowIndex { unRelativeRowIndex :: Int }


newtype ColIndex = ColIndex { unColIndex :: Int }
-}
