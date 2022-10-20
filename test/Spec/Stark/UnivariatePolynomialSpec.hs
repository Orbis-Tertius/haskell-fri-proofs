{-# LANGUAGE OverloadedStrings #-}

module Spec.Stark.UnivariatePolynomialSpec (testUnivariatePolynomial) where

import Control.Monad (forM_)
import Data.List (nub)
import Hedgehog
  ( Property,
    assert,
    forAll,
    property,
    (===),
  )
import Hedgehog.Gen (list)
import qualified Hedgehog.Range as Range
import Spec.Gen (genScalar)
import Stark.UnivariatePolynomial (areColinear, evaluate, interpolate)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

testUnivariatePolynomial :: TestTree
testUnivariatePolynomial =
  testGroup
    "UnivariatePolynomial"
    [ testPropertyNamed "evaluates to the correct values" "propInterpolate" propInterpolate,
      testPropertyNamed "recognizes colinear points" "propRecognizesColinear" propRecognizesColinear,
      testPropertyNamed "rejects non-colinear points" "propRejectsNonColinear" propRejectsNonColinear
    ]

propInterpolate :: Property
propInterpolate = property $ do
  points <- forAll (zip <$> (nub <$> list (Range.linear 1 10) genScalar) <*> list (Range.linear 1 10) genScalar)
  let p = interpolate points
  forM_ points (\(x, y) -> evaluate p x === y)

propRecognizesColinear :: Property
propRecognizesColinear = property $ do
  (m, b, xs) <- forAll ((,,) <$> genScalar <*> genScalar <*> list (Range.linear 1 10) genScalar)
  let y x = m * x + b
      ys = y <$> xs
  assert $ areColinear $ zip xs ys

propRejectsNonColinear :: Property
propRejectsNonColinear = property $ do
  (m, b, x', xs) <-
    forAll
      ( (,,,) <$> genScalar <*> genScalar <*> genScalar
          <*> ((:) <$> genScalar <*> list (Range.linear 1 10) genScalar)
      )
  let y x = m * x + b
      ys = y <$> xs
  assert $ not $ areColinear $ (x', y x' + 1) : zip xs ys
