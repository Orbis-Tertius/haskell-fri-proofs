module Plonk.Types.Z2
  ( Z2 (..)
  ) where


import           Data.Group                     (Group (invert))
import           Data.Kind                      (Type)
import           Data.Ratio                     ((%))
import           Math.Algebra.Polynomial.Class  (Ring)
import           Math.Algebra.Polynomial.Misc   (IsSigned (signOf), Sign (Plus))
import           Math.Algebra.Polynomial.Pretty (Pretty (pretty))
type Z2 :: Type
data Z2 = Zero | One
 deriving stock (Eq, Ord, Show)

instance IsSigned Z2 where
  signOf _ = pure Plus

instance Pretty Z2 where
  pretty Zero = "0"
  pretty One  = "1"

instance Ring Z2 where

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

instance Fractional Z2 where
  recip :: Z2 -> Z2
  recip Zero = Zero
  recip One  = One

instance Semigroup Z2 where
  (<>) :: Z2 -> Z2 -> Z2
  Zero <> Zero = Zero
  Zero <> One  = One
  One <> Zero  = One

instance Monoid Z2 where
  mempty :: Z2
  mempty = Zero  

instance Group Z2 where
  invert :: Z2 -> Z2
  invert Zero = Zero
  invert One = One
