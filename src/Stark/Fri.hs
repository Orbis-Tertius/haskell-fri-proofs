module Stark.Fri
  ( numRounds
  , evalDomain
  , sampleIndex
  , sampleIndices
  ) where


import Data.Bits (shift, xor)
import Data.ByteString (ByteString, unpack)

import Stark.Fri.Types (DomainLength (..), ExpansionFactor (..), NumColinearityTests (..), Offset (..), Omega (..), RandomSeed (..), ListSize (..), ReducedListSize (..), Index (..), SampleSize (..))
import Stark.Types.Scalar (Scalar)


numRounds :: DomainLength -> ExpansionFactor -> NumColinearityTests -> Int
numRounds (DomainLength d) (ExpansionFactor e) (NumColinearityTests n) =
  if fromIntegral d > e && 4 * n < d
  then 1 + numRounds
           (DomainLength (d `div` 2))
           (ExpansionFactor e)
           (NumColinearityTests n)
  else 0


evalDomain :: Offset -> Omega -> DomainLength -> [Scalar]
evalDomain (Offset o) (Omega m) (DomainLength d)
  = [o * (m ^ i) | i <- [0..d-1]]


sampleIndex :: ByteString -> Index -> Index
sampleIndex bs len =
  foldl (\acc b -> (acc `shift` 8) `xor` fromIntegral b) 0 (unpack bs)
  `mod` len


sampleIndices :: RandomSeed -> ListSize -> ReducedListSize -> SampleSize -> [Index]
sampleIndices = undefined
