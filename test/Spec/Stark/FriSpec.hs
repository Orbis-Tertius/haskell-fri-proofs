{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}


module Spec.Stark.FriSpec ( testFri ) where


import Control.Lens ((^.))
import           Hedgehog            (Property, forAll, property, (===))
import           Spec.Gen            (genFriConfiguration, genLowDegreePoly,
                                      genProofStream, genScalar)
import Stark.UnivariatePolynomial (interpolate, degree)
import           Stark.Fri           (getCodeword, prove, verify, evalDomain, splitAndFold, getMaxDegree)
import           Test.Tasty          (TestTree, testGroup, localOption)
import           Test.Tasty.Hedgehog (testPropertyNamed, HedgehogShrinkLimit (HedgehogShrinkLimit))
import Stark.Fri.Types (Challenge (Challenge), Codeword (unCodeword), DomainLength (DomainLength))


testFri :: TestTree
testFri = localOption (HedgehogShrinkLimit (Just 0))
  $ testGroup "Fri" [
  testPropertyNamed "Split and fold: preserves low-degreeness" "propSplitAndFold" propSplitAndFold,
  testPropertyNamed "Soundness: rejects invalid proofs" "propSoundness" propSoundness,
  testPropertyNamed "Completeness: true statements are accepted" "propCompleteness" propCompleteness
  ]

propSplitAndFold :: Property
propSplitAndFold = property $ do
  config <- forAll genFriConfiguration
  poly <- forAll (genLowDegreePoly config)
  alpha <- Challenge <$> forAll genScalar
  let c = getCodeword config poly
      c' = splitAndFold (config ^. #omega) (config ^. #offset) c alpha
      two :: Int = 2
      d' = evalDomain ((config ^. #offset) ^ two)
             ((config ^. #omega) ^ two)
             (DomainLength 32)
      poly' = interpolate (zip d' (unCodeword c'))
      m = getMaxDegree (config ^. #domainLength)
  max (degree poly') m === m

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
