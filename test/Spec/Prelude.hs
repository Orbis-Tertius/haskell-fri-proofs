module Spec.Prelude
  ( module Data.GenValidity
  , module Test.QuickCheck
  , module Test.Syd
  , module Test.Syd.Validity
  ) where


import           Data.GenValidity  (GenValid (..), genValidStructurally,
                                    shrinkValidStructurally)
import           Test.QuickCheck   (Gen, choose, chooseAny, chooseInt, elements,
                                    forAll, listOf, listOf1, oneof, vectorOf)

import           Test.Syd          (Spec, before, describe, it, modifyMaxSize,
                                    shouldBe, shouldNotBe, shouldNotSatisfy,
                                    shouldSatisfy)
import           Test.Syd.Validity (forAllValid)
