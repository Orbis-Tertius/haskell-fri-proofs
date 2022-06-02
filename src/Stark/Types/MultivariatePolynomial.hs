{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}


module Stark.Types.MultivariatePolynomial ( MultivariatePolynomial ) where


import Data.Poly.Multi (VMultiPoly)
import GHC.TypeLits (Nat)

import Stark.Types.Scalar (Scalar)


type MultivariatePolynomial (n :: Nat) = VMultiPoly n Scalar
