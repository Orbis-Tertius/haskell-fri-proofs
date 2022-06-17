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
  , AY (AY, unAY)
  , BY (BY, unBY)
  , CY (CY, unCY)
  , Query (Query, unQuery)
  , ProofStream (ProofStream)
  , Challenge (Challenge, unChallenge)
  , PolynomialValues (PolynomialValues, unPolynomialValues)
  , FriConfiguration (FriConfiguration)
  ) where


import Codec.Serialise (Serialise)
import Data.Bits (Bits)
import Data.ByteString (ByteString)
import Data.Map (Map)
import GHC.Generics (Generic)

import Stark.Types.AuthPath (AuthPath)
import Stark.Types.Commitment (Commitment)
import Stark.Types.Scalar (Scalar)


newtype Offset = Offset { unOffset :: Scalar }
  deriving (Eq, Ord, Num)


newtype Omega = Omega { unOmega :: Scalar }
  deriving (Eq, Ord, Num)


newtype DomainLength = DomainLength { unDomainLength :: Int }
  deriving Generic


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


newtype AY = AY { unAY :: Scalar }
  deriving (Generic, Serialise)

newtype BY = BY { unBY :: Scalar}
  deriving (Generic, Serialise)

newtype CY = CY { unCY :: Scalar }
  deriving (Generic, Serialise)


newtype Query = Query { unQuery :: (AY, BY, CY) }
  deriving (Generic, Serialise)


data ProofStream =
  ProofStream
  { commitments :: [Commitment]
  , queries :: [Query]
  , codewords :: [Codeword]
  , authPaths :: [AuthPath]
  }
  deriving Generic

instance Serialise ProofStream


newtype Challenge = Challenge { unChallenge :: Scalar }
  deriving Serialise


newtype PolynomialValues = PolynomialValues { unPolynomialValues :: Map Index Scalar }
  deriving (Semigroup, Monoid)


data FriConfiguration =
  FriConfiguration
  { offset :: Offset
  , omega :: Omega
  , domainLength :: DomainLength
  , expansionFactor :: ExpansionFactor
  , numColinearityTests :: NumColinearityTests
  }
  deriving Generic
