module Plonk.FFT
  ( fft
  , padToNearestPowerOfTwo
  ) where

import           Data.Bits           (bit, countLeadingZeros,
                                      countTrailingZeros, finiteBitSize)
import           Data.Group          (Group, pow)
import qualified Data.List           as List
import           Plonk.Types.Circuit (Domain (Domain))

log2 :: Int -> Int
log2 x = floorLog + correction
  where
    floorLog = finiteBitSize x - 1 - countLeadingZeros x
    correction =
      if countTrailingZeros x < floorLog
        then 1
        else 0

split :: [a] -> ([a], [a])
split = foldr (\a (r1, r2) -> (a : r2, r1)) ([], [])

padToNearestPowerOfTwo :: Num f => [f] -> [f]
padToNearestPowerOfTwo [] = []
padToNearestPowerOfTwo xs = padToNearestPowerOfTwoOf (length xs) xs

padToNearestPowerOfTwoOf ::
  Num f =>
  -- | n
  Int ->
  -- | list which should have length <= 2^n
  [f] ->
  -- | list which will have length 2^n
  [f]
padToNearestPowerOfTwoOf i xs = xs ++ replicate padLength 0
  where
    padLength = nearestPowerOfTwo - length xs
    nearestPowerOfTwo = bit $ log2 i

fft
  :: Group k
  => Num k
  => Domain d k
  -> [k]
  -> [k]
fft (Domain omega_n) as =
  case length as of
    1 -> as
    n ->
      let (as0, as1) = split as
          y0 = fft (Domain omega_n) as0
          y1 = fft (Domain omega_n) as1
          omegas = map (pow (omega_n (log2 n))) [0 .. n]
       in combine y0 y1 omegas
  where
    combine :: Num a => [a] -> [a] -> [a] -> [a]
    combine y0 y1 omegas =
      (\xs -> map fst xs ++ map snd xs)
        $ map (\(yk0, yk1, currentOmega) -> (yk0 + currentOmega * yk1, yk0 - currentOmega * yk1))
        $ List.zip3 y0 y1 omegas
