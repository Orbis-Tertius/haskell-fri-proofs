module Stark.Cast
  ( intToInteger,
    intToRatio,
    word8ToInt,
    word8ToInteger,
    word64ToInteger,
    word64ToRatio,
    intToWord64,
    word8ToWord64,
    word64ToInt,
  )
where

import Data.Bits (toIntegralSized)
import Data.Maybe (fromMaybe)
import Data.Word (Word64, Word8)
import Die (die)

intToInteger :: Int -> Integer
intToInteger = fromMaybe (die "intToInteger partiality") . toIntegralSized

intToRatio :: Int -> Rational
intToRatio = integerToRatio . fromMaybe (die "intToRatio partiality") . toIntegralSized

integerToRatio :: Integer -> Rational
integerToRatio = toRational

word8ToInt :: Word8 -> Int
word8ToInt = fromMaybe (die "word8ToInt partiality") . toIntegralSized

word8ToInteger :: Word8 -> Integer
word8ToInteger = fromMaybe (die "word8ToInteger partiality") . toIntegralSized

word64ToInteger :: Word64 -> Integer
word64ToInteger = fromMaybe (die "word64ToInteger partiality") . toIntegralSized

word64ToRatio :: Word64 -> Rational
word64ToRatio = integerToRatio . fromMaybe (die "word64ToRatio partiality") . toIntegralSized

intToWord64 :: Int -> Word64
intToWord64 = fromMaybe (die "intToWord64 partiality") . toIntegralSized

word8ToWord64 :: Word8 -> Word64
word8ToWord64 = fromMaybe (die "word8ToWord64 partiality") . toIntegralSized

word64ToInt :: Word64 -> Int
word64ToInt x =
  if 0 <= x && x < (2 ^ (63 :: Int))
    then fromMaybe (die "word64ToInt partiality") (toIntegralSized x)
    else die "called word64ToInt out of range"
