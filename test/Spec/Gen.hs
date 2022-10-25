{-# LANGUAGE OverloadedLabels #-}

module Spec.Gen
  ( genFriConfiguration,
    defaultFriConfiguration,
    genTranscript,
    genCodeword,
    genQuery,
    genAuthPath,
    genCommitment,
    genCapCommitment,
    genByteString,
    genScalar,
    genLowDegreePoly,
    genBinaryTree,
  )
where

import Control.Lens ((^.))
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.Generics.Labels ()
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (Last))
import Data.Word (Word64)
import Die (die)
import Hedgehog (Gen)
import Hedgehog.Gen
  ( bytes,
    choice,
    enum,
    list,
    word64,
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Math.Algebra.Polynomial.FreeModule (FreeMod (FreeMod))
import Math.Algebra.Polynomial.Univariate (U (U), Univariate (Uni))
import qualified Stark.BinaryTree as BinaryTree
import Stark.Cast (word64ToInt)
import Stark.Fri (FriResponse (Commit, LastCodeword', QueryRound), getMaxLowDegree)
import Stark.Fri.Types
  ( A (A),
    AuthPaths (AuthPaths),
    B (B),
    C (C),
    Codeword (Codeword),
    DomainLength (DomainLength),
    ExpansionFactor (ExpansionFactor),
    FriConfiguration (FriConfiguration),
    NumColinearityTests (NumColinearityTests),
    Offset (Offset),
    Omega (Omega),
    Query (Query),
  )
import Stark.Types.AuthPath (AuthPath (AuthPath))
import Stark.Types.BinaryTree (BinaryTree)
import Stark.Types.CapCommitment (CapCommitment (CapCommitment))
import Stark.Types.CapLength (CapLength (CapLength))
import Stark.Types.Commitment (Commitment (Commitment))
import Stark.Types.FiatShamir (Transcript (Transcript))
import Stark.Types.Index (Index (Index))
import Stark.Types.MerkleHash (MerkleHash (MerkleHash))
import Stark.Types.Scalar
  ( Scalar,
    fromWord64,
    order,
    primitiveNthRoot,
  )
import Stark.Types.UnivariatePolynomial (UnivariatePolynomial (UnivariatePolynomial))

genFriConfiguration :: Gen FriConfiguration
genFriConfiguration = defaultFriConfiguration . CapLength . (2 ^) <$> enum (0 :: Int) 0

defaultFriConfiguration :: CapLength -> FriConfiguration
defaultFriConfiguration =
  FriConfiguration
    (Offset 1)
    (Omega . fromMaybe (die "could not find omega") $ primitiveNthRoot dl)
    (DomainLength dl)
    (ExpansionFactor 2)
    (NumColinearityTests 1)
  where
    dl :: Word64
    dl = 64

genTranscript :: FriConfiguration -> Gen (Transcript FriResponse)
genTranscript config =
  Transcript <$> list (Range.linear 0 100) (genFriResponse config)

genFriResponse :: FriConfiguration -> Gen FriResponse
genFriResponse config =
  choice
    [ Commit <$> genCapCommitment,
      LastCodeword' . Last <$> Gen.maybe (genCodeword config),
      QueryRound
        <$> ( (,) <$> Gen.list (Range.linear 0 10) genQuery
                <*> Gen.list (Range.linear 0 10) genAuthPaths
            )
    ]

genAuthPaths :: Gen AuthPaths
genAuthPaths =
  AuthPaths
    <$> ( (,,) <$> (A <$> genAuthPath)
            <*> (B <$> genAuthPath)
            <*> (C <$> genAuthPath)
        )

genCommitment :: Gen Commitment
genCommitment = Commitment . MerkleHash <$> genByteString

genCapCommitment :: Gen CapCommitment
genCapCommitment = do
  n :: Int <- (2 ^) <$> enum (0 :: Int) 6
  CapCommitment . fromMaybe (die "could not generate binary tree")
    . BinaryTree.fromList
    <$> list (Range.singleton n) genCommitment

genQuery :: Gen Query
genQuery =
  Query
    <$> ( (,,)
            <$> (A <$> ((,) <$> genIndex <*> genScalar))
            <*> (B <$> ((,) <$> genIndex <*> genScalar))
            <*> (C <$> ((,) <$> genIndex <*> genScalar))
        )

genIndex :: Gen Index
genIndex = Index <$> Gen.integral (Range.linear 0 10)

genAuthPath :: Gen AuthPath
genAuthPath =
  AuthPath
    <$> list
      (Range.linear 1 10)
      ( (,,) <$> (Commitment . MerkleHash <$> genByteString)
          <*> (Commitment . MerkleHash <$> genByteString)
          <*> (Commitment . MerkleHash <$> genByteString)
      )

genByteString :: Gen ByteString
genByteString = bytes (Range.linear 1 10)

genCodeword :: FriConfiguration -> Gen Codeword
genCodeword config =
  Codeword
    <$> replicateM
      (word64ToInt (config ^. #domainLength . #unDomainLength))
      genScalar

genLowDegreePoly :: FriConfiguration -> Gen (UnivariatePolynomial Scalar)
genLowDegreePoly config = do
  let maxDegree = getMaxLowDegree (config ^. #domainLength) (config ^. #expansionFactor)
  coefs <- list (Range.singleton maxDegree) genScalar
  let monos :: [U x]
      monos = U <$> [0 .. maxDegree - 1]
  pure . UnivariatePolynomial . Uni . FreeMod . Map.fromList
    . filter ((/= 0) . snd)
    $ zip monos coefs

genScalar :: Gen Scalar
genScalar = do
  w <- word64 (Range.linear 0 (order - 1))
  maybe genScalar pure (fromWord64 w)

genBinaryTreeSize :: Gen Word64
genBinaryTreeSize = (2 ^) <$> enum (1 :: Word64) 8

genBinaryTree :: Gen a -> Gen (Word64, [a], BinaryTree a)
genBinaryTree g = do
  n <- genBinaryTreeSize
  xs <- list (Range.singleton (word64ToInt n)) g
  pure
    ( n,
      xs,
      fromMaybe (die "failed to generate binary tree")
        . BinaryTree.fromList
        $ xs
    )
