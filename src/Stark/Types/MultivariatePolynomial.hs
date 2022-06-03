{-# LANGUAGE DataKinds #-}


module Stark.Types.MultivariatePolynomial ( MultivariatePolynomial ) where


import Math.Algebra.Polynomial.Multivariate.Infinite (Poly)

import Stark.Types.Scalar (Scalar)


type MultivariatePolynomial = Poly Scalar "x"
