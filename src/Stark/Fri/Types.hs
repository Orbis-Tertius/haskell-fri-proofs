{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Stark.Fri.Types
  ( Offset (Offset, unOffset)
  , Omega (Omega, unOmega)
  , DomainLength (DomainLength, unDomainLength)
  , ExpansionFactor (ExpansionFactor, unExpansionFactor)
  , NumColinearityTests (NumColinearityTests, unNumColinearityTests)
  , ListSize (ListSize, unListSize)
  , RandomSeed (RandomSeed, unRandomSeed)
  , ReducedListSize (ReducedListSize, unReducedListSize)
  , SampleSize (SampleSize, unSampleSize)
  , ReducedIndex (ReducedIndex, unReducedIndex)
  , Codeword (Codeword, unCodeword)
  , A (A, unA)
  , B (B, unB)
  , C (C, unC)
  , Query (Query, unQuery)
  , AuthPaths (AuthPaths, unAuthPaths)
  , ProofStream (ProofStream)
  , Challenge (Challenge, unChallenge)
  , PolynomialValues (PolynomialValues, unPolynomialValues)
  , FriConfiguration (FriConfiguration)
  ) where


import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Map (Map)
import GHC.Generics (Generic)

import Stark.Types.AuthPath (AuthPath)
import Stark.Types.Commitment (Commitment)
import Stark.Types.Index (Index)
import Stark.Types.Scalar (Scalar)


newtype Offset = Offset { unOffset :: Scalar }
  deriving (Eq, Ord, Num, Show)


newtype Omega = Omega { unOmega :: Scalar }
  deriving (Eq, Ord, Num, Show, Generic)


newtype DomainLength = DomainLength { unDomainLength :: Int }
  deriving (Generic, Show)


newtype ExpansionFactor = ExpansionFactor { unExpansionFactor :: Rational }
  deriving (Show)


newtype NumColinearityTests = NumColinearityTests { unNumColinearityTests :: Int }
  deriving (Show)


newtype ListSize = ListSize { unListSize :: Int }


newtype RandomSeed = RandomSeed { unRandomSeed :: ByteString }


newtype ReducedListSize = ReducedListSize { unReducedListSize :: Int }


newtype SampleSize = SampleSize { unSampleSize :: Int }


newtype ReducedIndex = ReducedIndex { unReducedIndex :: Int }
  deriving (Eq, Ord)


newtype Codeword = Codeword { unCodeword :: [Scalar] }
  deriving (Eq, Serialise, Show)


newtype A a = A { unA :: a }
  deriving (Eq, Generic, Serialise, Show)

newtype B a = B { unB :: a }
  deriving (Eq, Generic, Serialise, Show)

newtype C a = C { unC :: a }
  deriving (Eq, Generic, Serialise, Show)


type ABC a = (A a, B a, C a)


newtype Query = Query { unQuery :: ABC Scalar }
  deriving (Eq, Generic, Serialise, Show)


newtype AuthPaths = AuthPaths { unAuthPaths :: ABC AuthPath }
  deriving (Eq, Generic, Serialise, Show)


data ProofStream =
  ProofStream
  { commitments :: [Commitment]
  , queries :: [Query]
  , lastCodeword :: Maybe Codeword
  , authPaths :: [[AuthPaths]]
  }
  deriving (Eq, Generic, Show)

instance Serialise ProofStream


newtype Challenge = Challenge { unChallenge :: Scalar }
  deriving Serialise


newtype PolynomialValues = PolynomialValues { unPolynomialValues :: Map Index Scalar }
  deriving (Eq, Semigroup, Monoid, Show)


data FriConfiguration =
  FriConfiguration
  { offset :: Offset
  , omega :: Omega
  , domainLength :: DomainLength
  , expansionFactor :: ExpansionFactor
  , numColinearityTests :: NumColinearityTests
  }
  deriving (Generic, Show)
