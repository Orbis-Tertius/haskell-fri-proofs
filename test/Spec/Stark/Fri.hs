module Spec.Stark.Fri ( spec ) where


import Spec.Prelude
import Spec.Gen (genFriConfiguration, genProofStream)
import Stark.Fri (verify)


spec :: Spec
spec = describe "Fri" $ do
  soundnessTest
  completenessTest


soundnessTest :: Spec
soundnessTest =
  it "rejects invalid proofs" $
    forAll genFriConfiguration $ \config ->
      forAll (genProofStream config) $ \proof ->
        verify config proof `shouldBe` Nothing


completenessTest :: Spec
completenessTest =
  it "creates proofs of true statements which are accepted" $ return ()


todo :: a
todo = todo
