module Plonk.Types.Vect
  ( Vect (..),
    toList,
  )
where

import Data.Kind (Type)
import Data.Vinyl.TypeLevel (Nat (S, Z))

type Vect :: Nat -> Type -> Type
data Vect :: Nat -> Type -> Type where
  Nil :: Vect 'Z a
  (:-) :: a -> Vect n a -> Vect ('S n) a

infixr 7 :-

instance Functor (Vect m) where
  fmap _ Nil = Nil
  fmap f (x :- xs) = f x :- fmap f xs

toList :: Vect n a -> [a]
toList Nil = []
toList (x :- xs) = x : toList xs
