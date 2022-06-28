module Spec.Stark.MerkleTreeSpec ( spec ) where


import Spec.Gen (genBinaryTree, genScalar, genCapCommitment, genAuthPath)
import Spec.Prelude
import Stark.MerkleTree (commit, open, verify)
import Stark.Prelude (uncurry4)
import Stark.Types.CapLength (CapLength (..))
import Stark.Types.Index (Index (..))


-- TODO: generate and test non-trivial cap lengths
capLength :: CapLength
capLength = 1


spec :: Spec
spec = describe "MerkleTree" $ do
  it "successfully verifies openings of commitments" $
    forAll (genBinaryTree genScalar) $ \(s, xs, t) ->
      forAll (Index <$> choose (0, s-1)) $ \i ->
        let c = commit capLength t
            p = open capLength i t
            x = xs !! unIndex i
        in (c, i, p, x) `shouldSatisfy` uncurry4 (verify capLength)

  it "rejects random inputs" $
    forAll ((,,,) <$> genCapCommitment
                  <*> (Index <$> choose (0, 1024))
                  <*> genAuthPath
                  <*> genScalar)
      $ (`shouldNotSatisfy` uncurry4 (verify capLength))
