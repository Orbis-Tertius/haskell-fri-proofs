{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}

module Spec.Stark.FriSpec ( spec ) where


import Spec.Prelude
import Spec.Gen (genFriConfiguration, genProofStream, genLowDegreePoly)
import Stark.Fri (verify, prove, getCodeword, emptyProofStream)


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
  it "creates proofs of true statements which are accepted" $
    forAll genFriConfiguration $ \config ->
      forAll (genLowDegreePoly config) $ \poly -> 
        verify config (fst (prove config (getCodeword config poly))) `shouldNotBe` Nothing
