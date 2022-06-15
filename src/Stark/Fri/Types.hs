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
  , Index (Index, unIndex)
  , ReducedIndex (ReducedIndex, unReducedIndex)
  , Codeword (Codeword, unCodeword)
  , ProofStream (ProofStream)
  , Challenge (Challenge, unChallenge)
  , FriConfiguration (FriConfiguration)
  ) where


import Codec.Serialise (Serialise)
import Data.Bits (Bits)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)

import Stark.Types.AuthPath (AuthPath)
import Stark.Types.Commitment (Commitment)
import Stark.Types.Scalar (Scalar)


newtype Offset = Offset { unOffset :: Scalar }
  deriving (Eq, Ord, Num)


newtype Omega = Omega { unOmega :: Scalar }
  deriving (Eq, Ord, Num)


newtype DomainLength = DomainLength { unDomainLength :: Int }


newtype ExpansionFactor = ExpansionFactor { unExpansionFactor :: Rational }


newtype NumColinearityTests = NumColinearityTests { unNumColinearityTests :: Int }


newtype ListSize = ListSize { unListSize :: Int }


newtype RandomSeed = RandomSeed { unRandomSeed :: ByteString }


newtype ReducedListSize = ReducedListSize { unReducedListSize :: Int }


newtype SampleSize = SampleSize { unSampleSize :: Int }


newtype Index = Index { unIndex :: Int }
  deriving (Eq, Ord, Num, Enum, Real, Integral, Bits)


newtype ReducedIndex = ReducedIndex { unReducedIndex :: Int }
  deriving (Eq, Ord)


newtype Codeword = Codeword { unCodeword :: [Scalar] }
  deriving Serialise


data ProofStream =
  ProofStream
  { commitments :: [Commitment]
  , codewords :: [Codeword]
  , authPaths :: [AuthPath]
  }
  deriving Generic

instance Serialise ProofStream


newtype Challenge = Challenge { unChallenge :: Scalar }
  deriving Serialise


data FriConfiguration =
  FriConfiguration
  Offset
  Omega
  DomainLength
  ExpansionFactor
  NumColinearityTests
