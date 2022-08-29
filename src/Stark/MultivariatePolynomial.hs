module Stark.MultivariatePolynomial
  ( isZero
  , constant
  , fromUnivariate
  , evaluate
  , evaluateSymbolic
  , evaluateSymbolicToUni
  ) where


import Data.Map (elems, singleton, mapKeys)
import Math.Algebra.Polynomial.Class (Polynomial (evalP, subsP))
import Math.Algebra.Polynomial.FreeModule (FreeMod (FreeMod, unFreeMod))
import Math.Algebra.Polynomial.Monomial.Generic (Monom (Monom))
import Math.Algebra.Polynomial.Pretty (Pretty)
import Math.Algebra.Polynomial.Univariate (unUni, U (U))
import Math.Algebra.Polynomial.Multivariate.Generic (Poly (Poly), unPoly)

import Stark.Types.MultivariatePolynomial (MultivariatePolynomial)
import Stark.Types.Scalar (Scalar)
import Stark.Types.UnivariatePolynomial (UnivariatePolynomial (UnivariatePolynomial))
import qualified Stark.UnivariatePolynomial as Uni


isZero :: MultivariatePolynomial a -> Bool
isZero = all (== 0) . elems . unFreeMod . unPoly


constant :: Ord a => Scalar -> MultivariatePolynomial a
constant coef = Poly (FreeMod (singleton (Monom mempty) coef))


fromUnivariate :: Ord a => UnivariatePolynomial Scalar -> a -> MultivariatePolynomial a
fromUnivariate (UnivariatePolynomial p) x =
  Poly . FreeMod . mapKeys (Monom . singleton x . unU) . unFreeMod . unUni $ p
  where
    unU :: U x -> Int
    unU (U y) = y


evaluate :: Ord a => Pretty a => MultivariatePolynomial a -> (a -> Scalar) -> Scalar
evaluate p xs = evalP id xs p


evaluateSymbolic :: Ord a => Pretty a
                 => MultivariatePolynomial a
                 -> (a -> MultivariatePolynomial a)
                 -> MultivariatePolynomial a
evaluateSymbolic p qs = subsP qs p


evaluateSymbolicToUni
  :: Ord a => Pretty a
  => MultivariatePolynomial a
  -> (a -> UnivariatePolynomial Scalar)
  -> UnivariatePolynomial Scalar
evaluateSymbolicToUni p qs = evalP Uni.constant qs p
