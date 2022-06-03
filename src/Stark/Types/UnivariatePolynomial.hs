{-# LANGUAGE DataKinds #-}


module Stark.Types.UnivariatePolynomial ( UnivariatePolynomial ) where


import Math.Algebra.Polynomial.Univariate (Univariate)

import Stark.Types.Scalar (Scalar)


type UnivariatePolynomial = Univariate Scalar "x"
