{-# LANGUAGE UndecidableInstances #-}


module Plonk.Types.Circuit
  ( Length
  , CircuitShape (..)
  , CircuitM (CircuitM)
  , Circuit
  , Constraint
  , GateConstraint (..)
  , RelativeCellRef (..)
  , RelativeRowIndex (..)
  , ColIndex (..)
  , Z2 (..)
  , Domain (..)
  , DomainGenerator (..)
  , Exponent (..)
  , Challenge (..)
  , example
  , exampleGC
  , HasData(..)
  , Entry
  , FAI(..)
  , ColType (..)
  , DegreeBound
  ) where


import           Data.Functor.Compose                         (Compose (Compose))
import           Data.Functor.Const                           (Const)
import           Data.Functor.Identity                        (Identity (Identity))
import           Data.Kind                                    (Constraint, Type)
import           Data.Vinyl.TypeLevel                         (Nat (S, Z))
import           GHC.Generics                                 (Generic)
import qualified Math.Algebra.Polynomial.Multivariate.Generic as Multi
import  Math.Algebra.Polynomial.Monomial.Generic (singletonMonom)
import Math.Algebra.Polynomial.FreeModule (singleton)
import           Math.Algebra.Polynomial.Pretty               (Pretty (pretty))
import           Plonk.Types.Fin                              (Fin(FZ))
import           Plonk.Types.Vect                             (Vect (Nil, (:-)))
import Data.Ratio ((%))

type Length :: [b] -> Nat
type family Length (a :: [b]) :: Nat
type instance (Length '[]) = 'Z
type instance Length (_ ': as) = 'S (Length as)

type FAI :: Type
data FAI = Instance | Advice | Fixed

type EN :: Type
data EN = EqCon | NEqCon

type ColType :: Type
data ColType = MkCol FAI EN


type NumCols :: Type
type NumCols = Nat


type DegreeBound :: Type
type DegreeBound = Nat


type GateConstraint :: NumCols -> DegreeBound -> Type -> Type
newtype GateConstraint n d a where
  MkGateConstraint :: { unGateConstraint :: Multi.Poly a (RelativeCellRef n) } -> GateConstraint n d a

type RelativeCellRef :: NumCols -> Type
data RelativeCellRef n where
  MkRelativeCellRef :: RelativeRowIndex -> ColIndex n -> RelativeCellRef n
  deriving stock (Eq, Ord, Show)

instance Pretty (RelativeCellRef n) where
  pretty = show

type ColIndex :: NumCols -> Type
newtype ColIndex n = ColIndex { unColIndex :: Fin n }
  deriving stock (Eq, Ord, Show)


type RelativeRowIndex :: Type
newtype RelativeRowIndex = RelativeRowIndex { unRelativeRowIndex :: Int }
  deriving stock (Eq, Ord, Show)


type Exponent :: Type
newtype Exponent = Exponent { unExponent :: Int }


type NumRows :: Type
type NumRows = Nat


type CircuitShape
  :: (Type -> Type)
  -> [ColType]
  -> HasData
  -> DegreeBound
  -> Type
  -> Type
data CircuitShape f ps h d a where
  CNil :: CircuitShape f '[] h d a
  (:&) :: (Compose f (Entry h j)) a -> CircuitShape f ps h d a -> CircuitShape f ('MkCol j e : ps) h d a

type HasData :: Type
data HasData where
  WithData :: HasData
  WithNoData :: HasData

type Entry :: HasData -> FAI -> (Type -> Type)
type family Entry (y :: HasData) (x :: FAI) :: Type -> Type where
  Entry _ 'Fixed    = Identity
  Entry 'WithData _ = Identity
  Entry _ _         = Const ()

type CircuitM
  :: (Type -> Type)
  -> [ColType]
  -> HasData
  -> DegreeBound
  -> Type
  -> Type
data CircuitM f ps h d a =
  CircuitM
  { shape       :: CircuitShape f ps h d a
  , constraints :: [GateConstraint (Length ps) d a]
  }
  deriving stock Generic

type Circuit :: [ColType] -> HasData -> NumRows -> DegreeBound -> Type -> Type
type Circuit ps h m = CircuitM (Vect m) ps h

infixr 7 :&

type MyC :: [ColType]
type MyC = '[ 'MkCol 'Instance 'EqCon, 'MkCol 'Advice 'NEqCon, 'MkCol 'Fixed 'EqCon, 'MkCol 'Fixed 'EqCon ]

type Z2 :: Type
data Z2 = Zero | One
 deriving stock (Eq, Show)

instance Num Z2 where
 (+) :: Z2 -> Z2 -> Z2
 Zero + Zero = Zero
 Zero + One = One
 One + Zero = One
 One + One = Zero

 (*) :: Z2 -> Z2 -> Z2
 Zero * Zero = Zero
 Zero * One = Zero
 One * Zero = Zero
 One * One = One

 (-) :: Z2 -> Z2 -> Z2
 Zero - Zero = Zero
 One - Zero = One
 Zero - One = One
 One - One = Zero

 negate :: Z2 -> Z2
 negate = id

 abs :: Z2 -> Z2
 abs = id

 signum :: Z2 -> Z2
 signum = id

 fromInteger :: Integer -> Z2
 fromInteger x = if (x % 2) == 0 then Zero else One

example :: CircuitShape (Vect ('S ('S ('S 'Z)))) MyC 'WithData d Z2
example = Compose (Identity One  :- Identity Zero :- Identity One :- Nil)
       :& Compose (Identity Zero :- Identity One  :- Identity Zero :- Nil)
       :& Compose (Identity One  :- Identity Zero :- Identity Zero :- Nil)
       :& Compose (Identity One  :- Identity Zero :- Identity Zero :- Nil)
       :& CNil


exampleGC :: [GateConstraint ('S ('S ('S 'Z))) ('S ('S 'Z)) Z2]
exampleGC = [MkGateConstraint $ Multi.Poly (singleton (singletonMonom (MkRelativeCellRef (RelativeRowIndex 0) (ColIndex FZ)) 1) One)]


type Domain :: NumRows -> Type -> Type
newtype Domain d a = Domain (DomainGenerator a)


type DomainGenerator :: Type -> Type
newtype DomainGenerator a = DomainGenerator { unDomainGenerator :: a }


type Challenge :: Type -> Type
newtype Challenge a = Challenge { unChallenge :: a }
