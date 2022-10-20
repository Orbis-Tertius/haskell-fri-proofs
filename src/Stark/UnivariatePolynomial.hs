module Stark.UnivariatePolynomial
  ( degree,
    isZero,
    leadingCoefficient,
    evaluate,
    interpolate,
    constant,
    linear,
    areColinear,
  )
where

import Control.Arrow ((***))
import qualified Data.FiniteField.PrimeField as PrimeField
import Data.Map
  ( elems,
    lookupMax,
    singleton,
  )
import Math.Algebra.Polynomial.Class
  ( Polynomial (evalP),
    Ring,
  )
import Math.Algebra.Polynomial.FreeModule (FreeMod (FreeMod, unFreeMod))
import Math.Algebra.Polynomial.Univariate
  ( U (U),
    Univariate (Uni),
    fromQUni,
    unUni,
  )
import Math.Algebra.Polynomial.Univariate.Lagrange (lagrangeInterp)
import Stark.Types.Scalar (Scalar (unScalar))
import Stark.Types.UnivariatePolynomial (UnivariatePolynomial (UnivariatePolynomial, unUnivariatePolynomial))

degree :: UnivariatePolynomial a -> Int
degree (UnivariatePolynomial p) =
  case lookupMax (unFreeMod (unUni p)) of
    Just (U i, _) -> i
    Nothing -> -1

isZero :: Eq a => Num a => UnivariatePolynomial a -> Bool
isZero = all (== 0) . elems . unFreeMod . unUni . unUnivariatePolynomial

leadingCoefficient :: UnivariatePolynomial a -> Maybe a
leadingCoefficient (UnivariatePolynomial p) = snd <$> lookupMax (unFreeMod (unUni p))

evaluate :: Ring a => UnivariatePolynomial a -> a -> a
evaluate (UnivariatePolynomial p) x = evalP id (const x) p

linear :: Scalar -> UnivariatePolynomial Scalar
linear coef = UnivariatePolynomial (Uni (FreeMod (singleton (U 1) coef)))

constant :: Scalar -> UnivariatePolynomial Scalar
constant coef = UnivariatePolynomial (Uni (FreeMod (singleton (U 0) coef)))

interpolate :: [(Scalar, Scalar)] -> UnivariatePolynomial Scalar
interpolate f = UnivariatePolynomial . fromQUni $ lagrangeInterp ((g *** g) <$> f)
  where
    g :: Scalar -> Rational
    g = fromIntegral . PrimeField.toInteger . unScalar

areColinear :: [(Scalar, Scalar)] -> Bool
areColinear = (< 2) . degree . interpolate
