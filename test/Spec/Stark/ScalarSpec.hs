{-# LANGUAGE OverloadedStrings #-}


module Spec.Stark.ScalarSpec ( testScalar ) where


import Hedgehog (Property, forAll, property, (===))
import Hedgehog.Gen (integral)
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Spec.Gen (genScalar)
import Stark.Cast (word64ToInteger)
import Stark.Types.Scalar (order, scalarToInteger, inverseScalar, primitiveNthRoot, toWord64)


testScalar :: TestTree
testScalar = testGroup "Scalar" [
    testPropertyNamed "addition is correct" "propAddScalar" propAddScalar
  , testPropertyNamed "negation is correct" "propNegateScalar" propNegateScalar
  , testPropertyNamed "multiplication is correct" "propMulScalar" propMulScalar
  , testPropertyNamed "inverse is correct" "propInverseScalar" propInverseScalar
  , testPropertyNamed "primitive nth root is correct" "propPrimitiveNthRoot" propPrimitiveNthRoot
  , testPropertyNamed "toWord64 preserve equality" "propToWord64PreservesEq" propToWord64PreservesEq
  ]


propAddScalar :: Property
propAddScalar = property $ do
  a <- forAll genScalar
  b <- forAll genScalar
  let a' = scalarToInteger a
      b' = scalarToInteger b
      c' = (a' + b') `mod` word64ToInteger order
  scalarToInteger (a + b) === c'


propNegateScalar :: Property
propNegateScalar = property $ do
  a <- forAll genScalar
  (a + negate a) === 0


propMulScalar :: Property
propMulScalar = property $ do
  a <- forAll genScalar
  b <- forAll genScalar
  let a' = scalarToInteger a
      b' = scalarToInteger b
      c' = (a' * b') `mod` word64ToInteger order
  scalarToInteger (a * b) === c'


propInverseScalar :: Property
propInverseScalar = property $ do
  a <- forAll genScalar
  if a == 0
    then inverseScalar a === Nothing
    else ((a *) <$> inverseScalar a) === Just 1


propPrimitiveNthRoot :: Property
propPrimitiveNthRoot = property $ do
  n :: Int <- forAll (integral (Range.linear 0 32))
  let m = 2 ^ n
  ((^ m) <$> primitiveNthRoot m) === Just 1


propToWord64PreservesEq :: Property
propToWord64PreservesEq = property $ do
  a <- forAll genScalar
  b <- forAll genScalar
  (a == b) === (toWord64 a == toWord64 b)
