{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Stark.Types.Scalar ( Scalar (Scalar, unScalar) ) where


import Prelude hiding (toInteger)
import Codec.Serialise (Serialise (encode, decode))
import Data.FiniteField.PrimeField (PrimeField, toInteger)
import GHC.Generics (Generic)
import Math.Algebra.Polynomial.Class (Ring)
import Math.Algebra.Polynomial.Misc (IsSigned (signOf), Sign (Plus))
import Math.Algebra.Polynomial.Pretty (Pretty (pretty))


newtype Scalar = Scalar { unScalar :: PrimeField 270497897142230380135924736767050121217 }
  deriving (Eq, Ord, Num, Fractional, Show, Generic)

instance Serialise Scalar where
  encode = encode . toInteger . unScalar
  decode = (fromIntegral :: Integer -> Scalar) <$> decode

instance IsSigned Scalar where
  signOf = const (Just Plus)

instance Pretty Scalar where
  pretty = show . unScalar

instance Ring Scalar where
