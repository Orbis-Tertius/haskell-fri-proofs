module Stark.Cast
  ( intToInteger
  , intToRatio
  , word8ToInt
  , word8ToInteger
  , word64ToInteger
  , word64ToRatio
  , intToWord64
  , word8ToWord64
  , word64ToInt
  ) where


import           Data.Word (Word64, Word8)


intToInteger :: Int -> Integer
intToInteger = fromIntegral


intToRatio :: Int -> Rational
intToRatio = fromIntegral


word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral


word8ToInteger :: Word8 -> Integer
word8ToInteger = fromIntegral


word64ToInteger :: Word64 -> Integer
word64ToInteger = fromIntegral


word64ToRatio :: Word64 -> Rational
word64ToRatio = fromIntegral


intToWord64 :: Int -> Word64
intToWord64 = fromIntegral


word8ToWord64 :: Word8 -> Word64
word8ToWord64 = fromIntegral


word64ToInt :: Word64 -> Int
word64ToInt x =
  if 0 <= x && x < (2 ^ (63 :: Int))
  then fromIntegral x
  else error "called word64ToInt out of range"
