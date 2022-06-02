module Stark.FiniteField
  ( cardinality
  , generator
  , primitiveNthRoot
  ) where


import Control.Monad (guard)
import Data.Bits (Bits ((.&.)))

import Stark.Types.Scalar (Scalar (Scalar))


cardinality :: Integer
cardinality = 270497897142230380135924736767050121217


generator :: Scalar
generator = 85408008396924667383611388730472331217


primitiveNthRoot :: Integer -> Maybe Scalar
primitiveNthRoot n = do
  guard (n <= (2 ^ (119 :: Integer)))
  guard (n .&. (n-1) == 0)
  return . Scalar $ f initRoot initOrder

  where
    initRoot = 85408008396924667383611388730472331217

    initOrder = 2 ^ (119 :: Integer)

    f root order =
      if order /= n
        then f (root * root) (order `quot` 2)
        else root
