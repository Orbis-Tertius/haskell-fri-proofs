{-# LANGUAGE DataKinds #-}


module Stark.Types.MultivariatePolynomial ( MultivariatePolynomial ) where


import           Math.Algebra.Polynomial.Multivariate.Generic (Poly)

import           Data.Kind                                    (Type)
import           Stark.Types.Scalar                           (Scalar)

type MultivariatePolynomial :: Type -> Type
type MultivariatePolynomial v = Poly Scalar v
