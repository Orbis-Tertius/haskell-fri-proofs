module Spec.Prelude
  ( module Test.QuickCheck
  ) where


import           Test.QuickCheck   (Gen, choose, chooseAny, chooseInt, elements,
                                    forAll, listOf, listOf1, oneof, vectorOf)
