module Stark.Cast
  ( intToInteger
  , intToRatio
  , word8ToInt
  , word8ToInteger
  , word64ToInteger
  , word64ToRatio
  ) where


import Data.Word (Word8, Word64)


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
