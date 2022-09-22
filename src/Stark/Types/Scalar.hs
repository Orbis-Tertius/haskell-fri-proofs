{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

-- Choose an appropriate finite field class from some library
module Stark.Types.Scalar
  ( Scalar
  , epsilon
  , order
  , fromWord64
  , toWord64
  , fromWord64Wrapping
  , inverseScalar
  , generator
  , powerOfTwoSubgroupRootOfUnity
  , rootOfUnity
  , sample
  ) where

import Data.Ratio (numerator, denominator)
import Codec.Serialise (Serialise)
import Math.Algebra.Polynomial.Class (Ring)
import Math.Algebra.Polynomial.Misc (IsSigned (signOf), Sign (Plus))
import Math.Algebra.Polynomial.Pretty (Pretty (pretty))
import qualified Data.ByteString as BS

import Data.Bits ((.&.), shiftR)
import Data.Word (Word64)
import Basement.Types.Word128 (Word128 (Word128))

{- |
  Finite field of order 2^64 - 2^32 + 1, or equivalently, 2^64 - 0xFFFFFFFF.
  The idea of this representation is taken from these sources:
  - Plonky2: https://github.com/mir-protocol/plonky2/blob/e127d5a4b11a9d5074b25d1d2dd2b765f404fbe3/field/src/goldilocks_field.rs
  - Some blog post: https://www.craig-wood.com/nick/armprime/math/
  The notable thing about this representation is that _all_ 'Word64's are a valid 'Scalar',
  but they don't always mean the same thing.
  Up until (not including) 2^64 - 0xFFFFFFFF, 'Word64' and 'Scalar' have the same meaning.
  Howevere, in the case of 'Scalar', from 2^64 - 0xFFFFFFFF and upto (not including) 2^64, it wraps around.
  This means that we have e.g. 2 0s in 'Scalar'. One at 0, and one at 2^64 - 0xFFFFFFFF.
  Thus, when we need the _canonical_ representation, we need to _reduce_ it, which essentially
  means: if less than the order, keep as is, otherwise, add 0xFFFFFFFF.
  We have some other nice properties as noted in the blog post, and similar tricks
  can be used for the various arithmetic operators.
-}
newtype Scalar = Scalar Word64
  deriving (Show, Serialise)

epsilon :: Word64
epsilon = 0xFFFFFFFF

order :: Word64
order = negate epsilon -- equivalent to 2^64 - epsilon

fromWord64 :: Word64 -> Maybe Scalar
fromWord64 x | x < order = Just $ Scalar x
fromWord64 _ = Nothing

fromWord64Wrapping :: Word64 -> Scalar
fromWord64Wrapping = Scalar

toWord64 :: Scalar -> Word64
toWord64 (Scalar x) =
  if x < order
    then x
    else x + epsilon

up :: Word64 -> Word128
up = Word128 0

-- would be great if we could check for overflow easily
addScalar :: Scalar -> Scalar -> Scalar
addScalar (toWord64 -> x) (toWord64 -> y) =
  let r = x + y in
  Scalar $ if r < x || r < y
    -- if we've underflowed, we've skipped `epsilon` amount, so we must add it back
    then r + epsilon
    else r

negateScalar :: Scalar -> Scalar
negateScalar (Scalar x) = Scalar $ order - x -- very simple

-- FIXME: Why is this algorithm correct? Give an informal proof.
mulScalar :: Scalar -> Scalar -> Scalar
mulScalar (Scalar x) (Scalar y) =
  case up x * up y of
    Word128 hi lo ->
      let
        hihi = shiftR hi 32
        hilo = hi .&. epsilon
        Word128 borrow t0 = up lo - up hihi
        t1 = if borrow > 0 then t0 - epsilon else t0
       in
        Scalar t1 + Scalar (hilo * epsilon)

inverseScalar :: Scalar -> Maybe Scalar
inverseScalar (Scalar 0) = Nothing
inverseScalar (Scalar ((+ epsilon) -> 0)) = Nothing
inverseScalar x = Just $ x^(order - 2)

generator :: Scalar
generator = Scalar 7

-- p = 2^64 - 0xFFFFFFFF
-- p = 2^64 - 2^32 + 1
-- p = 2^32 * (2^32 - 1) + 1
-- p - 1 = 2^S * T
-- S = 32
-- T = 2^32 - 1
powerOfTwoSubgroupRootOfUnity :: Scalar
powerOfTwoSubgroupRootOfUnity = generator ^ (2^(32 :: Integer) - 1 :: Integer)

rootOfUnity :: Integer -> Scalar
rootOfUnity n = generator ^ (5 * (fromIntegral order - 1) `div` n)

instance IsSigned Scalar where
  signOf = const $ Just Plus

instance Pretty Scalar where
  pretty = show . toWord64

instance Ring Scalar where

instance Num Scalar where
  fromInteger n = case fromWord64 . fromInteger $ n of
    Just n' -> n'
    Nothing -> error (show n <> " is not less than " <> show order)
  (+) = addScalar
  (*) = mulScalar
  negate = negateScalar
  abs = error "unimplemented"
  signum = error "unimplemented"

instance Fractional Scalar where
  recip x = case inverseScalar x of
    Just y -> y
    Nothing -> error "0 has no reciprocal"
  fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

instance Eq Scalar where
  x == y = toWord64 x == toWord64 y
instance Enum Scalar where
  toEnum n = case fromWord64 . toEnum $ n of
    Just n' -> n'
    Nothing -> error "out of bounds"
  fromEnum = fromEnum . toWord64
instance Ord Scalar where
  compare x y = compare (toWord64 x) (toWord64 y)
instance Real Scalar where
  toRational = toRational . toWord64
instance Integral Scalar where
  toInteger = toInteger . toWord64
  quotRem = error "unimplemented"

sample :: BS.ByteString -> Scalar
sample = fromInteger . sampleInteger

sampleInteger :: BS.ByteString -> Integer
sampleInteger = BS.foldl (\x b -> x + fromIntegral b) 0

