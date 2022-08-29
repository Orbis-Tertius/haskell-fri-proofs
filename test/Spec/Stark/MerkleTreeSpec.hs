{-# LANGUAGE ScopedTypeVariables #-}


module Spec.Stark.MerkleTreeSpec ( spec ) where


import Hedgehog (Property)
import           Spec.Gen              (genAuthPath, genBinaryTree,
                                        genCapCommitment, genScalar)
import           Spec.Prelude
import           Stark.MerkleTree      (commit, open, verify)
import           Stark.Prelude         (uncurry4)
import           Stark.Types.CapLength (CapLength (..))
import           Stark.Types.Index     (Index (..))
import Test.Tasty (TestTree)


testMerkleTree :: TestTree
testMerkleTree = testGroup "Merkle Tree" [
  testPropertynamed "successfully verifies openings of commitments" "propVerifiesOpenings" propVerifiesOpenings,
  testPropertynamed "rejects random inputs" "propRejectsRandomInputs" propRejectsRandomInputs
  ]


propVerifiesOpenings :: Property
ptopVerifiesOpenings = property $ do
  forAll (genBinaryTree genScalar) $ \(s, xs, t) ->
    forAll (choose (0, round (logBase (2 :: Double) (fromIntegral s)))) $ \(n :: Int) ->
      let capLength = 2 ^ n
      in forAll (Index <$> choose (0, s-1)) $ \i ->
        let c = commit capLength t
            p = open capLength i t
            x = xs !! unIndex i
        in (c, i, p, x) `shouldSatisfy` uncurry4 (verify capLength)

propRejectsRandomInputs :: Property
propRejectsRandomInputs = property $ do
  forAll (CapLength . (2^) <$> choose (0 :: Int, 8)) $ \capLength ->
    forAll ((,,,) <$> genCapCommitment
                  <*> (Index <$> choose (0, 1024))
                  <*> genAuthPath
                  <*> genScalar)
      (`shouldNotSatisfy` uncurry4 (verify capLength))
