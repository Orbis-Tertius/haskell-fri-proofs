module Spec.Stark.MerkleTreeSpec ( spec ) where


import Spec.Gen (genBinaryTree, genScalar)
import Spec.Prelude
import Stark.MerkleTree (dataToLeaf, commit, open, verify)
import Stark.Prelude (uncurry4)
import Stark.Types.Index (Index (..))


spec :: Spec
spec = describe "MerkleTree" $ do
  it "successfully verifies openings of commitments" $
    forAll (genBinaryTree genScalar) $ \(s, xs, t) ->
      forAll (Index <$> choose (0, s-1)) $ \i ->
        let c = commit t
            p = open i t
            l = dataToLeaf (xs !! unIndex i)
        in (c, i, p, l) `shouldSatisfy` uncurry4 verify

  -- it "rejects random inputs" $ return ()
