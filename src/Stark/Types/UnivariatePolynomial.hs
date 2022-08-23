{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}


module Stark.Types.UnivariatePolynomial ( UnivariatePolynomial (..) ) where


import Math.Algebra.Polynomial.Class (AlmostPolynomial, Polynomial)
import Math.Algebra.Polynomial.FreeModule (FreeMod (FreeMod), FreeModule)
import Math.Algebra.Polynomial.Pretty (Pretty)
import Math.Algebra.Polynomial.Univariate (Univariate (Uni))


newtype UnivariatePolynomial a =
  UnivariatePolynomial
  { unUnivariatePolynomial :: Univariate a "x" }
  deriving (Eq, Ord, Num)
  deriving newtype (Pretty, FreeModule, AlmostPolynomial, Polynomial)

instance Functor UnivariatePolynomial where
  fmap f (UnivariatePolynomial (Uni (FreeMod p))) =
    UnivariatePolynomial (Uni (FreeMod (f <$> p)))
