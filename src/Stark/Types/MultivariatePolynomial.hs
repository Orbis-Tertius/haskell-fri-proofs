{-# LANGUAGE DataKinds #-}


module Stark.Types.MultivariatePolynomial ( MultivariatePolynomial ) where


import Math.Algebra.Polynomial.Multivariate.Generic (Poly)

import Stark.Types.Scalar (Scalar)


type MultivariatePolynomial v = Poly Scalar v
