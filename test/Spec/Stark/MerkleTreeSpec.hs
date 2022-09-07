{-# LANGUAGE OverloadedStrings   #-}

module Spec.Stark.MerkleTreeSpec ( testMerkleTree ) where


import           Hedgehog              (Property, assert, forAll, property)
import           Hedgehog.Gen          (enum)
import           Spec.Gen              (genAuthPath, genBinaryTree,
                                        genCapCommitment, genScalar)
import           Stark.MerkleTree      (commit, open, verify)
import           Stark.Prelude         (uncurry4)
import           Stark.Types.CapLength (CapLength (CapLength))
import           Stark.Types.Index     (Index (Index, unIndex))
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.Hedgehog   (testPropertyNamed)


testMerkleTree :: TestTree
testMerkleTree = testGroup "Merkle Tree" [
  testPropertyNamed "successfully verifies openings of commitments" "propVerifiesOpenings" propVerifiesOpenings,
  testPropertyNamed "rejects random inputs" "propRejectsRandomInputs" propRejectsRandomInputs
  ]


propVerifiesOpenings :: Property
propVerifiesOpenings = property $ do
  (s, xs, t) <- forAll (genBinaryTree genScalar)
  n <- forAll $ enum (0 :: Int) (round (logBase (2 :: Double) (fromIntegral s)))
  let capLength :: CapLength
      capLength = 2 ^ n
  i <- forAll (Index <$> enum 0 (s - 1))
  let c = commit capLength t
      p = open capLength i t
      x = xs !! unIndex i
  assert $ uncurry4 (verify capLength) (c, i, p ,x)

propRejectsRandomInputs :: Property
propRejectsRandomInputs = property $ do
  capLength <- forAll (CapLength . (2 ^) <$> enum (0 :: Int) 8)
  (c, i, p, x) <- forAll ((,,,) <$> genCapCommitment
                               <*> (Index <$> enum 0 1024)
                              <*> genAuthPath
                            <*> genScalar)
  assert $ not $ uncurry4 (verify capLength) (c, i, p ,x)
