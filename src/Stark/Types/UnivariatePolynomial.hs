{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Stark.Types.UnivariatePolynomial ( UnivariatePolynomial (..) ) where


import Math.Algebra.Polynomial.FreeModule (FreeMod (FreeMod))
import Math.Algebra.Polynomial.Univariate (Univariate (Uni))


newtype UnivariatePolynomial a =
  UnivariatePolynomial
  { unUnivariatePolynomial :: Univariate a "x" }
  deriving (Eq, Ord, Num)

instance Functor UnivariatePolynomial where
  fmap f (UnivariatePolynomial (Uni (FreeMod p))) =
    UnivariatePolynomial (Uni (FreeMod (f <$> p)))
