{-# LANGUAGE OverloadedStrings #-}
module Spec.Stark.FriSpec ( testFri ) where


import           Hedgehog            (Property, forAll, property, (===))
import           Spec.Gen            (genFriConfiguration, genLowDegreePoly,
                                      genProofStream)
import           Stark.Fri           (getCodeword, prove, verify)
import           Test.Tasty          (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testPropertyNamed)


testFri :: TestTree
testFri = testGroup "Fri" [
  testPropertyNamed "Soundness: rejects invalid proofs" "propSoundness" propSoundness,
  testPropertyNamed "Completeness: true statements are accepted" "propCompleteness" propCompleteness
  ]

propSoundness :: Property
propSoundness = property $ do
  config <- forAll genFriConfiguration
  proof <- forAll $ genProofStream config
  verify config proof === False

propCompleteness :: Property
propCompleteness = property $ do
  config <- forAll genFriConfiguration
  poly <- forAll (genLowDegreePoly config)
  verify config (fst (prove config (getCodeword config poly))) === True
