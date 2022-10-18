module Plonk.Types.Z2
  ( Z2 (..)
  , PrimeField (MkElem)
  ) where


import           Data.Group                     (Group (invert))
import           Data.Kind                      (Type)
import           Data.Ratio                     (denominator, numerator)
import           Data.Proxy                     (Proxy(Proxy))
import           GHC.TypeLits                   (KnownNat, Nat, natVal)
import           Math.Algebra.Polynomial.Class  (Ring)
import           Math.Algebra.Polynomial.Misc   (IsSigned (signOf), Sign (Plus))
import           Math.Algebra.Polynomial.Pretty (Pretty (pretty))
import Refined (LessThan, Refined, refine, unrefine)
import Refined.Unsafe (reallyUnsafeRefine)






{--
type Refined :: k -> Type -> Type
data Refined p x where
--}
  




type PrimeField :: Nat -> Type
data PrimeField a where
  MkElem :: Refined (LessThan a) Integer -> PrimeField a 
  deriving stock (Eq, Show)

instance KnownNat a => Num (PrimeField a) where
  (+) :: PrimeField a -> PrimeField a -> PrimeField a
  MkElem x + MkElem y = MkElem $ reallyUnsafeRefine $ (unrefine x + unrefine y) `mod` (natVal (Proxy @a))

  (-) :: PrimeField a -> PrimeField a -> PrimeField a
  MkElem x - MkElem y = MkElem $ reallyUnsafeRefine $ (unrefine x - unrefine y) `mod` (natVal (Proxy @a))

  (*) :: PrimeField a -> PrimeField a -> PrimeField a
  MkElem x * MkElem y = MkElem $ reallyUnsafeRefine $ (unrefine x * unrefine y) `mod` (natVal (Proxy @a))

  negate :: PrimeField a -> PrimeField a
  negate (MkElem x) = MkElem $ reallyUnsafeRefine $ natVal (Proxy @a) - unrefine x

  fromInteger :: Integer -> PrimeField a
  fromInteger x = MkElem $ reallyUnsafeRefine $ unrefine x `mod` (natVal (Proxy @a))

{--
instance KnownNat a => Fractional (PrimeField a) where
  recip :: PrimeField a -> PrimeField a
  recip x = undefined
--}

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
 fromInteger x = if even x then Zero else One

instance Fractional Z2 where
  recip :: Z2 -> Z2
  recip Zero = Zero
  recip One  = One

  fromRational :: Rational -> Z2
  fromRational x = fromInteger (numerator x) * recip (fromInteger $ denominator x)

instance Semigroup Z2 where
  (<>) :: Z2 -> Z2 -> Z2
  Zero <> Zero = Zero
  Zero <> One  = One
  One <> Zero  = One
  One <> One   = Zero

instance Monoid Z2 where
  mempty :: Z2
  mempty = Zero

instance Group Z2 where
  invert :: Z2 -> Z2
  invert Zero = Zero
  invert One  = One
