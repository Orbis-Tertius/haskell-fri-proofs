module Stark.Types.UnivariatePolynomial ( UnivariatePolynomial ) where


import Data.Poly (VPoly)

import Stark.Types.Scalar (Scalar)


type UnivariatePolynomial = VPoly Scalar
