{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Stark.Fri.Types
  ( Offset (Offset, unOffset),
    Omega (Omega, unOmega),
    DomainLength (DomainLength, unDomainLength),
    ExpansionFactor (ExpansionFactor, unExpansionFactor),
    NumColinearityTests (NumColinearityTests, unNumColinearityTests),
    ListSize (ListSize, unListSize),
    RandomSeed (RandomSeed, unRandomSeed),
    ReducedListSize (ReducedListSize, unReducedListSize),
    SampleSize (SampleSize, unSampleSize),
    ReducedIndex (ReducedIndex, unReducedIndex),
    Codeword (Codeword, unCodeword),
    A (A, unA),
    B (B, unB),
    C (C, unC),
    ABC,
    Query (Query, unQuery),
    AuthPaths (AuthPaths, unAuthPaths),
    Challenge (Challenge, unChallenge),
    FriConfiguration (FriConfiguration),
    randomSeed,
  )
where

import Codec.Serialise (Serialise, serialise)
import Data.ByteString (ByteString, toStrict)
import Data.Kind (Type)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Stark.Hash (hash)
import Stark.Types.AuthPath (AuthPath)
import Stark.Types.CapLength (CapLength)
import Stark.Types.FiatShamir (Sampleable (sample))
import Stark.Types.Scalar (Scalar)
import qualified Stark.Types.Scalar as Scalar

type Offset :: Type
newtype Offset = Offset {unOffset :: Scalar}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)

type Omega :: Type
newtype Omega = Omega {unOmega :: Scalar}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)

type DomainLength :: Type
newtype DomainLength = DomainLength {unDomainLength :: Word64}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Enum, Real, Integral)

type ExpansionFactor :: Type
newtype ExpansionFactor = ExpansionFactor {unExpansionFactor :: Int}
  deriving stock (Show)

type NumColinearityTests :: Type
newtype NumColinearityTests = NumColinearityTests {unNumColinearityTests :: Word64}
  deriving stock (Show)

type ListSize :: Type
newtype ListSize = ListSize {unListSize :: Word64}

type RandomSeed :: Type
newtype RandomSeed = RandomSeed {unRandomSeed :: ByteString}

type ReducedListSize :: Type
newtype ReducedListSize = ReducedListSize {unReducedListSize :: Word64}

type SampleSize :: Type
newtype SampleSize = SampleSize {unSampleSize :: Word64}

type ReducedIndex :: Type
newtype ReducedIndex = ReducedIndex {unReducedIndex :: Word64}
  deriving stock (Eq, Ord)

type Codeword :: Type
newtype Codeword = Codeword {unCodeword :: [Scalar]}
  deriving stock (Eq, Show)
  deriving newtype (Serialise)

type A :: Type -> Type
newtype A a = A {unA :: a}
  deriving stock (Eq, Generic, Show)
  deriving newtype (Serialise)

type B :: Type -> Type
newtype B a = B {unB :: a}
  deriving stock (Eq, Generic, Show)
  deriving newtype (Serialise)

type C :: Type -> Type
newtype C a = C {unC :: a}
  deriving stock (Eq, Generic, Show)
  deriving newtype (Serialise)

type ABC :: Type -> Type
type ABC a = (A a, B a, C a)

type Query :: Type
newtype Query = Query {unQuery :: ABC Scalar}
  deriving stock (Eq, Generic, Show)
  deriving newtype (Serialise)

type AuthPaths :: Type
newtype AuthPaths = AuthPaths {unAuthPaths :: ABC AuthPath}
  deriving stock (Eq, Generic, Show)
  deriving newtype (Serialise)

type Challenge :: Type
newtype Challenge = Challenge {unChallenge :: Scalar}
  deriving newtype (Serialise)

instance Sampleable Challenge where
  sample = Challenge . Scalar.sample . unRandomSeed . randomSeed

randomSeed :: Serialise a => a -> RandomSeed
randomSeed = RandomSeed . hash . toStrict . serialise

type FriConfiguration :: Type
data FriConfiguration = FriConfiguration
  { offset :: Offset,
    omega :: Omega,
    domainLength :: DomainLength,
    expansionFactor :: ExpansionFactor,
    numColinearityTests :: NumColinearityTests,
    capLength :: CapLength
  }
  deriving stock (Generic, Show)
