module Stark.UnivariatePolynomial
  ( degree
  , isZero
  , leadingCoefficient
  , evaluate
  ) where


import Data.Map (lookupMax)
import Math.Algebra.Polynomial.Class (Polynomial (evalP))
import Math.Algebra.Polynomial.FreeModule (FreeMod (unFreeMod))
import Math.Algebra.Polynomial.Univariate (unUni, U (U))

import Stark.Types.Scalar (Scalar)
import Stark.Types.UnivariatePolynomial (UnivariatePolynomial)


degree :: UnivariatePolynomial -> Int
degree p =
  case lookupMax (unFreeMod (unUni p)) of
    Just (U i, _) -> i
    Nothing -> -1


isZero :: UnivariatePolynomial -> Bool
isZero = (== Nothing) . lookupMax . unFreeMod . unUni


leadingCoefficient :: UnivariatePolynomial -> Maybe Scalar
leadingCoefficient p = snd <$> lookupMax (unFreeMod (unUni p))


evaluate :: UnivariatePolynomial -> Scalar -> Scalar
evaluate p x = evalP id (const x) p
