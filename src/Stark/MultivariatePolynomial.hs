module Stark.MultivariatePolynomial
  ( isZero
  , constant
  , linearBasis
  ) where


import Data.Map (elems, singleton)
import Math.Algebra.Polynomial.FreeModule (FreeMod (FreeMod, unFreeMod))
import Math.Algebra.Polynomial.Multivariate.Infinite (Poly (Poly), unPoly, XInf (XInf))

import Stark.Types.MultivariatePolynomial (MultivariatePolynomial)
import Stark.Types.Scalar (Scalar)


isZero :: MultivariatePolynomial -> Bool
isZero = all (== 0) . elems . unFreeMod . unPoly


constant :: Scalar -> MultivariatePolynomial
constant coef = Poly (FreeMod (singleton (XInf [0]) coef))


linearBasis :: Int -> [MultivariatePolynomial]
linearBasis n =
  Poly . FreeMod . flip singleton 1 . XInf <$>
  [ replicate (i-1) 0 ++ [1] ++ replicate (n-i) 0
  | i <- [1..n] ]
