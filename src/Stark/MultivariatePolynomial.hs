{-# LANGUAGE NoImplicitPrelude #-}


module Stark.MultivariatePolynomial
  ( isZero
  , constant
  , linearBasis
  , fromUnivariate
  , evaluate
  , evaluateSymbolic
  ) where


import Prelude hiding ((!!))
import Data.List.Safe ((!!))
import Data.Map (elems, singleton, mapKeys)
import Data.Maybe (fromMaybe)
import Math.Algebra.Polynomial.Class (Polynomial (evalP, subsP), fromIndex)
import Math.Algebra.Polynomial.FreeModule (FreeMod (FreeMod, unFreeMod))
import Math.Algebra.Polynomial.Univariate (unUni)
import Math.Algebra.Polynomial.Multivariate.Infinite (Poly (Poly), unPoly, XInf (XInf))

import Stark.Types.MultivariatePolynomial (MultivariatePolynomial)
import Stark.Types.Scalar (Scalar)
import Stark.Types.UnivariatePolynomial (UnivariatePolynomial)
import Stark.Types.Variable (Variable (Variable))


isZero :: MultivariatePolynomial -> Bool
isZero = all (== 0) . elems . unFreeMod . unPoly


constant :: Scalar -> MultivariatePolynomial
constant coef = Poly (FreeMod (singleton (XInf [0]) coef))


linearBasis :: Int -> [MultivariatePolynomial]
linearBasis n =
  Poly . FreeMod . flip singleton 1 . XInf <$>
  [ replicate (i-1) 0 ++ [1] ++ replicate (n-i) 0
  | i <- [1..n] ]


fromUnivariate :: UnivariatePolynomial -> Variable -> MultivariatePolynomial
fromUnivariate p (Variable i) =
  Poly . FreeMod . mapKeys (const x) . unFreeMod . unUni $ p
  where
    x :: XInf x
    x = XInf $ replicate (i-1) 0 ++ [1]


evaluate :: MultivariatePolynomial -> [Scalar] -> Scalar
evaluate p xs = evalP id (fromMaybe 0 . (xs !!) . fromIndex) p


evaluateSymbolic :: MultivariatePolynomial
                 -> [MultivariatePolynomial]
                 -> MultivariatePolynomial
evaluateSymbolic p qs = subsP (fromMaybe 0 . (qs !!) . fromIndex) p
