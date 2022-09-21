module Stark.Types.Scalar ( Scalar (Scalar, unScalar) ) where


import           Codec.Serialise                (Serialise (decode, encode))
import           Data.FiniteField.PrimeField    as P (PrimeField, toInteger)
import Data.Group (Group (invert))
import           Data.Kind                      (Type)
import           GHC.Generics                   (Generic)
import           Math.Algebra.Polynomial.Class  (Ring)
import           Math.Algebra.Polynomial.Misc   (IsSigned (signOf), Sign (Plus))
import           Math.Algebra.Polynomial.Pretty (Pretty (pretty))

type Scalar :: Type
newtype Scalar = Scalar { unScalar :: PrimeField 270497897142230380135924736767050121217 }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Fractional)

instance Semigroup Scalar where
  x <> y = x + y

instance Monoid Scalar where
  mempty = 0

instance Group Scalar where
  invert = negate

instance Serialise Scalar where
  encode = encode . P.toInteger . unScalar
  decode = (fromIntegral :: Integer -> Scalar) <$> decode

instance IsSigned Scalar where
  signOf = const (Just Plus)

instance Pretty Scalar where
  pretty = show . unScalar

instance Ring Scalar where
