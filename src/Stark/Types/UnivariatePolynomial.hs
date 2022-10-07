{-# LANGUAGE UndecidableInstances #-}

module Stark.Types.UnivariatePolynomial (UnivariatePolynomial (..)) where

import Data.Kind (Type)
import Math.Algebra.Polynomial.Class
  ( AlmostPolynomial,
    Polynomial,
  )
import Math.Algebra.Polynomial.FreeModule
  ( FreeMod (FreeMod),
    FreeModule,
  )
import Math.Algebra.Polynomial.Pretty (Pretty)
import Math.Algebra.Polynomial.Univariate (Univariate (Uni))

type UnivariatePolynomial :: Type -> Type
newtype UnivariatePolynomial a = UnivariatePolynomial
  {unUnivariatePolynomial :: Univariate a "x"}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Pretty, FreeModule, AlmostPolynomial, Polynomial)

instance Functor UnivariatePolynomial where
  fmap f (UnivariatePolynomial (Uni (FreeMod p))) =
    UnivariatePolynomial (Uni (FreeMod (f <$> p)))
