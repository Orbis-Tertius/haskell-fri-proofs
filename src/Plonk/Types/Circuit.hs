{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
  , Polynomial (..)
  , Monomial (..)
  , RelativeRowIndex (..)
  , ColIndex (..)
  , Z2 (..)
  , Domain (..)
  , DomainGenerator (..)
  , UnivariatePolynomial (..)
  , Exponent (..)
  , Challenge (..)
  , f
  , HasData(..)
  ) where


import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Const
import Data.Map (Map)
import Data.Type.Natural hiding (Zero)
import qualified Data.Type.Natural as N
import Data.Kind
import GHC.Generics (Generic)
import qualified GHC.TypeLits as TL


data Vect :: Nat -> Type -> Type where
  Nil :: Vect N.Zero a
  (:-) :: a -> Vect n a -> Vect (S n) a

infixr 7 :-

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
data Monomial a v d = Monomial a (Vect d (Maybe v))


type UnivariatePolynomial :: Type -> Type
newtype UnivariatePolynomial a
  = UnivariatePolynomial
  { unUnivariatePolynomial :: Map Exponent a }


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
  (:&) :: (Compose f (F h j)) a -> CircuitShape f ps h d a -> CircuitShape f (('MkCol j e) : ps) h d a

type HasData :: Type
data HasData where
  WithData :: HasData
  WithNoData :: HasData

type F :: HasData -> FAI -> (Type -> Type)
type family F (y :: HasData) (x :: FAI) :: Type -> Type where
  F _ 'Fixed    = Identity
  F 'WithData _ = Identity
  F _ _         = Const ()


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

f :: CircuitShape (Vect 3) MyC 'WithData d Z2
f =  Compose (Identity One  :- Identity Zero :- Identity One :- Nil)
  :& Compose (Identity Zero :- Identity One  :- Identity Zero :- Nil)
  :& Compose (Identity One  :- Identity Zero :- Identity Zero :- Nil)
  :& CNil


type Domain :: NumRows -> Type -> Type
newtype Domain d a = Domain (DomainGenerator a)


type DomainGenerator :: Type -> Type
newtype DomainGenerator a = DomainGenerator { unDomainGenerator :: a }


type Challenge :: Type -> Type
newtype Challenge a = Challenge { unChallenge :: a }
