{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Spec.Gen
  ( genFriConfiguration
  , defaultFriConfiguration
  , genProofStream
  , genCodeword
  , genQuery
  , genAuthPath
  , genCommitment
  , genCapCommitment
  , genByteString
  , genScalar
  , genLowDegreePoly
  , genBinaryTree
  ) where


import           Control.Lens                       ((^.))
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as BS
import           Data.Generics.Labels               ()
import qualified Data.Map                           as Map
import           Data.Maybe                         (fromMaybe)
import           Math.Algebra.Polynomial.FreeModule (FreeMod (FreeMod))
import           Math.Algebra.Polynomial.Univariate (U (..), Univariate (Uni))

import           Spec.Prelude
import qualified Stark.BinaryTree                   as BinaryTree
import           Stark.FiniteField                  (cardinality, generator,
                                                     primitiveNthRoot)
import           Stark.Fri                          (getMaxDegree)
import           Stark.Fri.Types                    (A (..), AuthPaths (..),
                                                     B (..), C (..),
                                                     Codeword (..),
                                                     DomainLength (..),
                                                     ExpansionFactor (..),
                                                     FriConfiguration (..),
                                                     NumColinearityTests (..),
                                                     Offset (..), Omega (..),
                                                     ProofStream (..),
                                                     Query (..))
import           Stark.Types.AuthPath               (AuthPath (..))
import           Stark.Types.BinaryTree             (BinaryTree)
import           Stark.Types.CapCommitment          (CapCommitment (..))
import           Stark.Types.CapLength              (CapLength (..))
import           Stark.Types.Commitment             (Commitment (..))
import           Stark.Types.MerkleHash             (MerkleHash (..))
import           Stark.Types.Scalar                 (Scalar (..))
import           Stark.Types.UnivariatePolynomial   (UnivariatePolynomial)


genFriConfiguration :: Gen FriConfiguration
genFriConfiguration = defaultFriConfiguration . CapLength . (2^) <$> choose (0 :: Int, 4)


defaultFriConfiguration :: CapLength -> FriConfiguration
defaultFriConfiguration =
   FriConfiguration
  (Offset generator)
  (Omega . fromMaybe (error "could not find omega") $ primitiveNthRoot (fromIntegral dl))
  (DomainLength dl)
  (ExpansionFactor 2)
  (NumColinearityTests 4)
  where
    dl = 256



genProofStream :: FriConfiguration -> Gen ProofStream
genProofStream config =
  ProofStream
  <$> listOf genCapCommitment
  <*> listOf (listOf genQuery)
  <*> oneof [pure Nothing, Just <$> genCodeword config]
  <*> listOf (listOf genAuthPaths)


genAuthPaths :: Gen AuthPaths
genAuthPaths =
  AuthPaths
  <$> ((,,) <$> (A <$> genAuthPath)
            <*> (B <$> genAuthPath)
            <*> (C <$> genAuthPath))


genCommitment :: Gen Commitment
genCommitment = Commitment . MerkleHash <$> genByteString


genCapCommitment :: Gen CapCommitment
genCapCommitment = do
  n :: Int <- (2^) <$> choose (0, 6 :: Int)
  CapCommitment . fromMaybe (error "could not generate binary tree")
    . BinaryTree.fromList <$> vectorOf n genCommitment


genQuery :: Gen Query
genQuery = Query <$> ((,,) <$> (A <$> genScalar) <*> (B <$> genScalar) <*> (C <$> genScalar))


genAuthPath :: Gen AuthPath
genAuthPath = AuthPath <$> listOf1 (Commitment . MerkleHash <$> genByteString)


genByteString :: Gen ByteString
genByteString = BS.pack <$> listOf chooseAny


genCodeword :: FriConfiguration -> Gen Codeword
genCodeword config =
  Codeword <$> replicateM
  (config ^. #domainLength . #unDomainLength)
  genScalar


genLowDegreePoly :: FriConfiguration -> Gen (UnivariatePolynomial a)
genLowDegreePoly config = do
  let maxDegree = getMaxDegree (config ^. #domainLength)
  coefs <- vectorOf maxDegree genScalar
  let monos = U <$> [0..maxDegree-1]
  pure . Uni . FreeMod . Map.fromList
    . filter ((/= 0) . snd) $ zip monos coefs


genScalar :: Gen Scalar
genScalar = Scalar . fromIntegral <$> choose (0, cardinality - 1)


genBinaryTreeSize :: Gen Int
genBinaryTreeSize = (2 ^) <$> chooseInt (1, 8)


genBinaryTree :: Gen a -> Gen (Int, [a], BinaryTree a)
genBinaryTree g = do
  n <- genBinaryTreeSize
  xs <- vectorOf n g
  return (n, xs,
    fromMaybe (error "failed to generate binary tree")
      . BinaryTree.fromList $ xs)
