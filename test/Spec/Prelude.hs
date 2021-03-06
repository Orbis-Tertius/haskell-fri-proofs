module Spec.Prelude
  ( module Data.GenValidity
  , module Test.QuickCheck
  , module Test.Syd
  , module Test.Syd.Validity
  ) where


import           Data.GenValidity  (GenValid (..), genValidStructurally,
                                    shrinkValidStructurally)
import           Test.QuickCheck   (Gen, choose, elements, forAll, listOf, listOf1,
                                    oneof, vectorOf, chooseAny, chooseInt)

import           Test.Syd          (Spec, before, describe, it, shouldBe, shouldNotBe, shouldSatisfy, shouldNotSatisfy, modifyMaxSize)
import           Test.Syd.Validity (forAllValid)
