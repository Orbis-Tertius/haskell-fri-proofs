{-# LANGUAGE DataKinds #-}
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
  , Circuit' (..)
  , Circuit
  , CircuitWithData'
  , CircuitWithData
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
  ) where


import Data.Map (Map)
import Data.Type.Natural hiding (Zero)
import qualified Data.Type.Natural as N
import Data.Kind
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
    -> DegreeBound
    -> Type
    -> Type
    -> Type where
  CNil :: CircuitShape f '[] d a b
  (:&) :: f a -> CircuitShape f ps d a b -> CircuitShape f (('MkCol 'Fixed e) : ps) d a b
  (:*) :: f b -> CircuitShape f ps d a b -> CircuitShape f (('MkCol 'Advice e) : ps) d a b
  (:^) :: f b -> CircuitShape f ps d a b -> CircuitShape f (('MkCol 'Instance e) : ps) d a b


type Circuit'
  :: (Type -> Type)
  -> [ColType]
  -> DegreeBound
  -> Type
  -> Type
  -> Type
data Circuit' f ps d a b =
  Circuit
  { shape :: CircuitShape f ps d a b
  , constraints :: [GateConstraint (Length ps) d a]
  }


type Circuit :: [ColType] -> NumRows -> DegreeBound -> Type -> Type
type Circuit ps m d a = Circuit' (Vect m) ps d a ()


type CircuitWithData'
  :: (Type -> Type)
  -> [ColType]
  -> DegreeBound
  -> Type
  -> Type
type CircuitWithData' f ps d a = Circuit' f ps d a a


type CircuitWithData :: [ColType] -> NumRows -> DegreeBound -> Type -> Type
type CircuitWithData ps m d a = Circuit' (Vect m) ps d a a


infixr 7 :&
infixr 7 :*
infixr 7 :^

type MyC :: [ColType]
type MyC = '[ 'MkCol 'Instance 'EqCon, 'MkCol 'Advice 'NEqCon, 'MkCol 'Fixed 'EqCon ]

data Z2 = Zero | One

f :: CircuitShape (Vect 3) MyC d Z2 Z2
f =  (One  :- Zero :- One :- Nil)
  :^ (Zero :- One  :- Zero :- Nil)
  :* (One  :- Zero :- Zero :- Nil)
  :& CNil


type Domain :: NumRows -> Type -> Type
newtype Domain d a = Domain (DomainGenerator a)


type DomainGenerator :: Type -> Type
newtype DomainGenerator a = DomainGenerator { unDomainGenerator :: a }


type Challenge :: Type -> Type
newtype Challenge a = Challenge { unChallenge :: a }
