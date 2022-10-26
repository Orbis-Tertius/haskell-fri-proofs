{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Stark.FriSpec (testFri, propCompleteness) where

import Control.Lens ((^.))
import Hedgehog (Property, forAll, property, (/==), (===), Seed (Seed), withSkip)
import Spec.Gen
  ( genFriConfiguration,
    genLowDegreePoly,
    genScalar,
    genTranscript,
  )
import Stark.Fri
  ( evalDomain,
    getCodeword,
    getMaxLowDegree,
    prove,
    splitAndFold,
    verify,
  )
import Stark.Fri.Types
  ( Challenge (Challenge),
    Codeword (unCodeword),
  )
import Stark.UnivariatePolynomial (degree, interpolate)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Hedgehog
  ( HedgehogShrinkLimit (HedgehogShrinkLimit),
    HedgehogTestLimit (HedgehogTestLimit),
    HedgehogReplay (HedgehogReplay),
    testPropertyNamed,
  )

testFri :: TestTree
testFri =
  localOption (HedgehogShrinkLimit (Just 0))
    . localOption (HedgehogTestLimit (Just 500))
    . localOption (HedgehogReplay (Just (10, (Seed 18112217981669132046 14647583382509377465))))
    $ testGroup
        "Fri"
        [ -- testPropertyNamed "Split and fold: preserves low-degreeness" "propSplitAndFold" propSplitAndFold,
          -- testPropertyNamed "Soundness: rejects invalid proofs" "propSoundness" propSoundness,
          testPropertyNamed "Completeness: true statements are accepted" "propCompleteness"
            $ withSkip "372:" propCompleteness
        ]

propSplitAndFold :: Property
propSplitAndFold = property $ do
  config <- forAll genFriConfiguration
  poly <- forAll (genLowDegreePoly config)
  alpha <- Challenge <$> forAll genScalar
  let c = getCodeword config poly
      d =
        evalDomain
          (config ^. #offset)
          (config ^. #omega)
          (config ^. #domainLength)
      polyI = interpolate (zip d (unCodeword c))
  poly === polyI
  let c' = splitAndFold (config ^. #omega) (config ^. #offset) c alpha
      dLength' = ((config ^. #domainLength) `div` 2)
      d' =
        evalDomain
          (config ^. #offset)
          ((config ^. #omega) ^ two)
          dLength'
      poly' = interpolate (zip d' (unCodeword c'))
      m' = getMaxLowDegree dLength' (config ^. #expansionFactor)
  max (degree poly') m' === m'
  where
    two :: Int
    two = 2

propSoundness :: Property
propSoundness = property $ do
  config <- forAll genFriConfiguration
  proof <- forAll $ genTranscript config
  verify config proof /== Right ()

propCompleteness :: Property
propCompleteness = property $ do
  config <- forAll genFriConfiguration
  poly <- forAll (genLowDegreePoly config)
  (verify config =<< prove config (getCodeword config poly)) === Right ()
