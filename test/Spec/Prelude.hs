module Spec.Prelude
  ( module Data.GenValidity
  , module Test.QuickCheck
  , module Test.Syd
  , module Test.Syd.Validity
  ) where


import           Data.GenValidity  (GenValid (..), genValidStructurally,
                                    shrinkValidStructurally)
import           Test.QuickCheck   (Gen, choose, elements, forAll, listOf,
                                    oneof, vectorOf, chooseAny)

import           Test.Syd          (Spec, before, describe, it, shouldBe, shouldNotBe)
import           Test.Syd.Validity (forAllValid)