{-# LANGUAGE OverloadedLabels #-}


module Spec.Stark.FriSpec ( spec ) where


import Spec.Prelude
import Spec.Gen (genFriConfiguration, genProofStream, genLowDegreePoly)
import Stark.Fri (verify, prove, getCodeword)


spec :: Spec
spec = describe "Fri" $ do
  soundnessTest
  completenessTest


soundnessTest :: Spec
soundnessTest =
  it "rejects invalid proofs" $
    forAll genFriConfiguration $ \config ->
      forAll (genProofStream config) $ \proof ->
        verify config proof `shouldBe` False


completenessTest :: Spec
completenessTest =
  it "creates proofs of true statements which are accepted" $
    forAll genFriConfiguration $ \config ->
      forAll (genLowDegreePoly config) $ \poly -> 
        verify config (fst (prove config (getCodeword config poly))) `shouldBe` True
