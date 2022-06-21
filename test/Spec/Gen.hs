{-# LANGUAGE OverloadedLabels #-}


module Spec.Gen
  ( genFriConfiguration
  , genProofStream
  , genCodeword
  , genQuery
  , genAuthPath
  , genCommitment
  , genByteString
  , genScalar
  ) where


import Control.Lens ((^.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Generics.Labels ()

import Spec.Prelude
import Stark.FiniteField (cardinality)
import Stark.Fri.Types (FriConfiguration (..), Codeword (..), ProofStream (..), AY (..), BY (..), CY (..), Query (..))
import Stark.Types.AuthPath (AuthPath (..))
import Stark.Types.Commitment (Commitment (..))
import Stark.Types.MerkleHash (MerkleHash (..))
import Stark.Types.Scalar (Scalar (..))


genFriConfiguration :: Gen FriConfiguration
genFriConfiguration = todo


genProofStream :: FriConfiguration -> Gen ProofStream
genProofStream config =
  ProofStream
  <$> listOf genCommitment
  <*> listOf genQuery
  <*> oneof [pure Nothing, Just <$> genCodeword config]
  <*> listOf genAuthPath


genCommitment :: Gen Commitment
genCommitment = Commitment . MerkleHash <$> genByteString


genQuery :: Gen Query
genQuery = Query <$> ((,,) <$> (AY <$> genScalar) <*> (BY <$> genScalar) <*> (CY <$> genScalar))


genAuthPath :: Gen AuthPath
genAuthPath = AuthPath <$> listOf (MerkleHash <$> genByteString)


genByteString :: Gen ByteString
genByteString = BS.pack <$> listOf chooseAny


genCodeword :: FriConfiguration -> Gen Codeword
genCodeword config =
  Codeword <$> (sequence $ replicate
  (config ^. #domainLength . #unDomainLength)
  genScalar)


genScalar :: Gen Scalar
genScalar = Scalar . fromIntegral <$> choose (0, cardinality - 1)


todo :: a
todo = todo
