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
  , genLowDegreePoly
  ) where


import Control.Lens ((^.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Generics.Labels ()
import Data.Maybe (fromMaybe)

import Spec.Prelude
import Stark.FiniteField (cardinality, generator, primitiveNthRoot)
import Stark.Fri.Types (FriConfiguration (..), Codeword (..), ProofStream (..), AY (..), BY (..), CY (..), Query (..), Offset (..), DomainLength (..), ExpansionFactor (..), NumColinearityTests (..), Omega (..))
import Stark.Fri (getMaxDegree)
import Stark.Types.AuthPath (AuthPath (..))
import Stark.Types.Commitment (Commitment (..))
import Stark.Types.MerkleHash (MerkleHash (..))
import Stark.Types.Scalar (Scalar (..))
import Stark.Types.UnivariatePolynomial (UnivariatePolynomial)


genFriConfiguration :: Gen FriConfiguration
genFriConfiguration =
  pure $ FriConfiguration
  (Offset generator)
  (Omega . fromMaybe (error "could not find omega") $ primitiveNthRoot (fromIntegral dl))
  (DomainLength dl)
  (ExpansionFactor 4)
  (NumColinearityTests 64)
  where
    dl = 32 -- TODO is this a good value?


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


genLowDegreePoly :: FriConfiguration -> Gen UnivariatePolynomial
genLowDegreePoly config =
  let maxDegree = getMaxDegree (config ^. #domainLength)
  in todo


todo :: a
todo = todo


genScalar :: Gen Scalar
genScalar = Scalar . fromIntegral <$> choose (0, cardinality - 1)
