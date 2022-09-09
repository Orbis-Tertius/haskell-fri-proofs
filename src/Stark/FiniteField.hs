{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}

-- Choose an appropriate finite field class from some library
module Stark.FiniteField
  ( Fp
  , epsilon
  , order
  , fromWord64
  , toWord64
  , fromWord64Wrapping
  , inverseFp
  , generator
  , powerOfTwoSubgroupRootOfUnity
  , rootOfUnity
  , sqrtFp
  ) where


import Data.Bits ((.&.), shiftR)
import Data.Word (Word64)
import Basement.Types.Word128 (Word128 (Word128))

{- |
  Finite field of order 2^64 - 2^32 + 1, or equivalently, 2^64 - 0xFFFFFFFF.
  The idea of this representation is taken from these sources:
  - Plonky2: https://github.com/mir-protocol/plonky2/blob/e127d5a4b11a9d5074b25d1d2dd2b765f404fbe3/field/src/goldilocks_field.rs
  - Some blog post: https://www.craig-wood.com/nick/armprime/math/
  The notable thing about this representation is that _all_ 'Word64's are a valid 'Fp',
  but they don't always mean the same thing.
  Up until (not including) 2^64 - 0xFFFFFFFF, 'Word64' and 'Fp' have the same meaning.
  Howevere, in the case of 'Fp', from 2^64 - 0xFFFFFFFF and upto (not including) 2^64, it wraps around.
  This means that we have e.g. 2 0s in 'Fp'. One at 0, and one at 2^64 - 0xFFFFFFFF.
  Thus, when we need the _canonical_ representation, we need to _reduce_ it, which essentially
  means: if less than the order, keep as is, otherwise, add 0xFFFFFFFF.
  We have some other nice properties as noted in the blog post, and similar tricks
  can be used for the various arithmetic operators.
-}
newtype Fp = Fp Word64
  deriving stock Show

epsilon :: Word64
epsilon = 0xFFFFFFFF

order :: Word64
order = negate epsilon -- equivalent to 2^64 - epsilon

fromWord64 :: Word64 -> Maybe Fp
fromWord64 x | x < order = Just $ Fp x
fromWord64 _ = Nothing

fromWord64Wrapping :: Word64 -> Fp
fromWord64Wrapping = Fp

toWord64 :: Fp -> Word64
toWord64 (Fp x) =
  if x < order
    then x
    else x + epsilon

up :: Word64 -> Word128
up = Word128 0

-- would be great if we could check for overflow easily
addFp :: Fp -> Fp -> Fp
addFp (toWord64 -> x) (toWord64 -> y) =
  let r = x + y in
  Fp $ if r < x || r < y
    -- if we've underflowed, we've skipped `epsilon` amount, so we must add it back
    then r + epsilon
    else r

negateFp :: Fp -> Fp
negateFp (Fp x) = Fp $ order - x -- very simple

-- FIXME: Why is this algorithm correct? Give an informal proof.
mulFp :: Fp -> Fp -> Fp
mulFp (Fp x) (Fp y) =
  case up x * up y of
    Word128 hi lo ->
      let
        hihi = shiftR hi 32
        hilo = hi .&. epsilon
        Word128 borrow t0 = up lo - up hihi
        t1 = if borrow > 0 then t0 - epsilon else t0
       in
        Fp t1 + Fp (hilo * epsilon)

inverseFp :: Fp -> Maybe Fp
inverseFp (Fp 0) = Nothing
inverseFp (Fp ((+ epsilon) -> 0)) = Nothing
inverseFp x = Just $ x^(order - 2)

sqrtFp :: Fp -> Maybe Fp
sqrtFp = error "unimplemented"

generator :: Fp
generator = Fp 7

-- p = 2^64 - 0xFFFFFFFF
-- p = 2^64 - 2^32 + 1
-- p = 2^32 * (2^32 - 1) + 1
-- p - 1 = 2^S * T
-- S = 32
-- T = 2^32 - 1
powerOfTwoSubgroupRootOfUnity :: Fp
powerOfTwoSubgroupRootOfUnity = generator ^ (2^(32 :: Integer) - 1 :: Integer)

rootOfUnity :: Integer -> Fp
rootOfUnity n = generator ^ (5 * (fromIntegral order - 1) `div` n)

instance Num Fp where
  fromInteger n = case fromWord64 . fromInteger $ n of
    Just n' -> n'
    Nothing -> error (show n <> " is not less than " <> show order)
  (+) = addFp
  (*) = mulFp
  negate = negateFp
  abs = error "unimplemented"
  signum = error "unimplemented"

instance Eq Fp where
  x == y = toWord64 x == toWord64 y
instance Enum Fp where
  toEnum n = case fromWord64 . toEnum $ n of
    Just n' -> n'
    Nothing -> error "out of bounds"
  fromEnum = fromEnum . toWord64
instance Ord Fp where
  compare x y = compare (toWord64 x) (toWord64 y)
instance Real Fp where
  toRational = toRational . toWord64
instance Integral Fp where
  toInteger = toInteger . toWord64
  quotRem = error "unimplemented"
