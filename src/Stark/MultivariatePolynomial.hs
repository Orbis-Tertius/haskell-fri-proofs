module Stark.MultivariatePolynomial
  ( isZero
  ) where


import Data.Map (elems)
import Math.Algebra.Polynomial.FreeModule (FreeMod (unFreeMod))
import Math.Algebra.Polynomial.Multivariate.Infinite (unPoly)

import Stark.Types.MultivariatePolynomial (MultivariatePolynomial)


isZero :: MultivariatePolynomial -> Bool
isZero = all (== 0) . elems . unFreeMod . unPoly
