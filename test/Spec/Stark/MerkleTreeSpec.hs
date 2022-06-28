module Spec.Stark.MerkleTreeSpec ( spec ) where


import Spec.Gen (genBinaryTree, genScalar, genCommitment, genAuthPath)
import Spec.Prelude
import Stark.MerkleTree (commit, open, verify)
import Stark.Prelude (uncurry4)
import Stark.Types.Index (Index (..))


spec :: Spec
spec = describe "MerkleTree" $ do
  it "successfully verifies openings of commitments" $
    forAll (genBinaryTree genScalar) $ \(s, xs, t) ->
      forAll (Index <$> choose (0, s-1)) $ \i ->
        let c = commit t
            p = open i t
            x = xs !! unIndex i
        in (c, i, p, x) `shouldSatisfy` uncurry4 verify

  it "rejects random inputs" $
    forAll ((,,,) <$> genCommitment
                  <*> (Index <$> choose (0, 1024))
                  <*> genAuthPath
                  <*> genScalar)
      $ (`shouldNotSatisfy` uncurry4 verify)
