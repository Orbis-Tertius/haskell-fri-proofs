{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Stark.Types.Scalar ( Scalar (Scalar, unScalar) ) where


import Math.Algebra.Polynomial.Class (Ring)
import Math.Algebra.Polynomial.Misc (IsSigned (signOf), Sign (Plus))
import Math.Algebra.Polynomial.Pretty (Pretty (pretty))
import Math.FiniteField.PrimeField.Generic (Fp)


newtype Scalar = Scalar { unScalar :: Fp 270497897142230380135924736767050121217 }
  deriving (Eq, Ord, Num, Show)

instance IsSigned Scalar where
  signOf = const (Just Plus)

instance Pretty Scalar where
  pretty = show . unScalar

instance Ring Scalar where
