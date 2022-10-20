{-# LANGUAGE UndecidableInstances #-}

module Spec.Plonk.Circuit (GenVect (genVect), GenCircuitShape (genCircuitShape)) where

import Data.Functor.Compose (Compose (Compose))
import Data.Kind (Constraint, Type)
import Data.Vinyl.TypeLevel (Nat (S, Z))
import Hedgehog (Gen)
import Plonk.Types.Circuit
  ( CircuitShape (CNil, (:&)),
    ColType (MkCol),
    DegreeBound,
    Entry,
    HasData,
  )
import Plonk.Types.Vect (Vect (Nil, (:-)))

type GenCircuitShape ::
  (Type -> Type) ->
  [ColType] ->
  HasData ->
  DegreeBound ->
  Type ->
  Constraint
class GenCircuitShape f ps h d a where
  genCircuitShape :: Gen (f a) -> Gen (CircuitShape f ps h d a)

type GenVect :: Nat -> Constraint
class GenVect m where
  genVect :: Gen a -> Gen (Vect m a)

instance GenVect 'Z where
  genVect _ = pure Nil

instance GenVect m => GenVect ('S m) where
  genVect f = (:-) <$> f <*> genVect @m f

instance GenCircuitShape (Vect m) '[] h d a where
  genCircuitShape _ = pure CNil

instance
  ( Functor f,
    Applicative (Entry h j),
    GenCircuitShape f ps h d a
  ) =>
  GenCircuitShape f ('MkCol j k ': ps) h d a
  where
  genCircuitShape f = (:&) <$> fmap (Compose . fmap pure) f <*> genCircuitShape f
