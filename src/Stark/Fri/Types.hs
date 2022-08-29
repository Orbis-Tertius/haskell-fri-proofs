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
  , FriConfiguration (FriConfiguration)
  ) where


import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)

import Stark.Types.AuthPath (AuthPath)
import Stark.Types.CapCommitment (CapCommitment)
import Stark.Types.CapLength (CapLength)
import Stark.Types.Scalar (Scalar)


newtype Offset = Offset { unOffset :: Scalar }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Num


newtype Omega = Omega { unOmega :: Scalar }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Num


newtype DomainLength = DomainLength { unDomainLength :: Int }
  deriving stock (Eq, Ord, Show, Generic)


newtype ExpansionFactor = ExpansionFactor { unExpansionFactor :: Rational }
  deriving stock (Show)


newtype NumColinearityTests = NumColinearityTests { unNumColinearityTests :: Int }
  deriving stock (Show)


newtype ListSize = ListSize { unListSize :: Int }


newtype RandomSeed = RandomSeed { unRandomSeed :: ByteString }


newtype ReducedListSize = ReducedListSize { unReducedListSize :: Int }


newtype SampleSize = SampleSize { unSampleSize :: Int }


newtype ReducedIndex = ReducedIndex { unReducedIndex :: Int }
  deriving stock (Eq, Ord)


newtype Codeword = Codeword { unCodeword :: [Scalar] }
  deriving stock (Eq, Show)
  deriving newtype Serialise


newtype A a = A { unA :: a }
  deriving stock (Eq, Generic, Show)
  deriving newtype Serialise

newtype B a = B { unB :: a }
  deriving stock (Eq, Generic, Show)
  deriving newtype Serialise

newtype C a = C { unC :: a }
  deriving stock (Eq, Generic, Show)
  deriving newtype Serialise


type ABC a = (A a, B a, C a)


newtype Query = Query { unQuery :: ABC Scalar }
  deriving stock (Eq, Generic, Show)
  deriving newtype Serialise


newtype AuthPaths = AuthPaths { unAuthPaths :: ABC AuthPath }
  deriving stock (Eq, Generic, Show)
  deriving newtype Serialise


data ProofStream =
  ProofStream
  { commitments :: [CapCommitment]
  , queries :: [[Query]]
  , lastCodeword :: Maybe Codeword
  , authPaths :: [[AuthPaths]]
  }
  deriving stock (Eq, Generic, Show)

instance Serialise ProofStream


newtype Challenge = Challenge { unChallenge :: Scalar }
  deriving newtype Serialise


data FriConfiguration =
  FriConfiguration
  { offset :: Offset
  , omega :: Omega
  , domainLength :: DomainLength
  , expansionFactor :: ExpansionFactor
  , numColinearityTests :: NumColinearityTests
  , capLength :: CapLength
  }
  deriving stock (Generic, Show)
