{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Stark.Types.Scalar ( Scalar (Scalar, unScalar) ) where


import Math.FiniteField.PrimeField.Generic (Fp)


newtype Scalar = Scalar { unScalar :: Fp 270497897142230380135924736767050121217 }
  deriving (Eq, Ord, Num)
