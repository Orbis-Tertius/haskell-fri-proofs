{-# LANGUAGE UndecidableInstances #-}
module Plonk.Types.Vect
  ( Vect (..)
  , toList
  ) where


import           Data.Kind            (Type)
import           Data.Vinyl.TypeLevel (Nat (S, Z))
import Data.Distributive (Distributive (distribute))
import Data.Functor.Rep (Representable (tabulate, index, type Rep), distributeRep)
import Plonk.Types.Fin (Fin(FZ, FS), finInj)

type Vect :: Nat -> Type -> Type
data Vect :: Nat -> Type -> Type where
  Nil :: Vect 'Z a
  (:-) :: a -> Vect n a -> Vect ('S n) a

infixr 7 :-

instance Functor (Vect m) where
  fmap _ Nil       = Nil
  fmap f (x :- xs) = f x :- fmap f xs

toList :: Vect n a -> [a]
toList Nil       = []
toList (x :- xs) = x : toList xs

instance Representable (Vect ('S n)) => Distributive (Vect ('S n)) where
  distribute :: Functor f => f (Vect ('S n) b) -> Vect ('S n) (f b)
  distribute = distributeRep

instance Representable (Vect n) => Representable (Vect ('S n)) where
  type Rep (Vect ('S n)) = Fin ('S n)

  tabulate :: (Fin ('S n) -> a) -> Vect ('S n) a
  tabulate f = _

  index :: Vect ('S n) a -> Fin ('S n) -> a
  index (x :- xs) FZ = x
  index (x :- xs) (FS a) = index @(Vect n) xs (unsafeCoerce @(Fin n) a)
