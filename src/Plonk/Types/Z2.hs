module Plonk.Types.Z2
  ( Z2 (..)
  ) where


import           Data.Kind  (Type)
import           Data.Ratio ((%))

type Z2 :: Type
data Z2 = Zero | One
 deriving stock (Eq, Show)

instance Num Z2 where
 (+) :: Z2 -> Z2 -> Z2
 Zero + Zero = Zero
 Zero + One  = One
 One + Zero  = One
 One + One   = Zero

 (*) :: Z2 -> Z2 -> Z2
 Zero * Zero = Zero
 Zero * One  = Zero
 One * Zero  = Zero
 One * One   = One

 (-) :: Z2 -> Z2 -> Z2
 Zero - Zero = Zero
 One - Zero  = One
 Zero - One  = One
 One - One   = Zero

 negate :: Z2 -> Z2
 negate = id

 abs :: Z2 -> Z2
 abs = id

 signum :: Z2 -> Z2
 signum = id

 fromInteger :: Integer -> Z2
 fromInteger x = if (x % 2) == 0 then Zero else One
