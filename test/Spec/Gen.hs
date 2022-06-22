{-# LANGUAGE OverloadedLabels #-}


module Spec.Gen
  ( genFriConfiguration
  , defaultFriConfiguration
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
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Math.Algebra.Polynomial.FreeModule (FreeMod (FreeMod))
import Math.Algebra.Polynomial.Univariate (U (..), Univariate (Uni))

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
genFriConfiguration = pure defaultFriConfiguration


-- defaultFriConfiguration :: FriConfiguration
-- defaultFriConfiguration =
--    FriConfiguration
--   (Offset generator)
--   (Omega . fromMaybe (error "could not find omega") $ primitiveNthRoot (fromIntegral dl))
--   (DomainLength dl)
--   (ExpansionFactor 4)
--   (NumColinearityTests 64)
--   where
--     dl = 1024 -- TODO is this a good value?


defaultFriConfiguration :: FriConfiguration
defaultFriConfiguration =
   FriConfiguration
  (Offset generator)
  (Omega . fromMaybe (error "could not find omega") $ primitiveNthRoot (fromIntegral dl))
  (DomainLength dl)
  (ExpansionFactor 2)
  (NumColinearityTests 2)
  where
    dl = 16



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
genLowDegreePoly config = do
  let maxDegree = getMaxDegree (config ^. #domainLength)
  coefs <- vectorOf maxDegree genScalar
  let monos = U <$> [0..maxDegree-1]
  pure . Uni . FreeMod . Map.fromList
    . filter ((/= 0) . snd) $ zip monos coefs


genScalar :: Gen Scalar
genScalar = Scalar . fromIntegral <$> choose (0, cardinality - 1)
