module Stark.FiniteField
  ( cardinality
  , generator
  , primitiveNthRoot
  , sample
  ) where


import           Control.Monad               (guard)
import           Data.Bits                   (Bits ((.&.)))
import qualified Data.ByteString             as BS

import           Data.FiniteField.PrimeField (PrimeField)
import           Stark.Types.Scalar          (Scalar (Scalar))


cardinality :: Integer
cardinality = 270497897142230380135924736767050121217


-- A generator of the multiplicative subgroup.
generator :: Scalar
generator = 85408008396924667383611388730472331217


primitiveNthRoot :: Integer -> Maybe Scalar
primitiveNthRoot n = do
  guard (n <= (2 ^ (119 :: Integer)))
  guard (n .&. (n-1) == 0)
  return . Scalar $ f initRoot initOrder

  where
    initRoot :: PrimeField 270497897142230380135924736767050121217
    initRoot = 85408008396924667383611388730472331217

    initOrder :: Integer
    initOrder = 2 ^ (119 :: Integer)

    f :: Num p => p -> Integer -> p
    f root order =
      if order /= n
        then f (root * root) (order `quot` 2)
        else root


sample :: BS.ByteString -> Scalar
sample = fromInteger . sampleInteger


sampleInteger :: BS.ByteString -> Integer
sampleInteger = BS.foldl (\x b -> x + fromIntegral b) 0
