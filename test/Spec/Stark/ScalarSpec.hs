{-# LANGUAGE OverloadedStrings #-}


module Spec.Stark.ScalarSpec ( testScalar ) where


import Data.Word (Word64)
import Hedgehog (Property, forAll, property, (===), (/==))
import Hedgehog.Gen (integral)
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Spec.Gen (genScalar)
import Stark.Cast (word64ToInteger)
import Stark.Types.Scalar (order, scalarToInteger, inverseScalar, primitiveNthRoot, toWord64, generator)


testScalar :: TestTree
testScalar = testGroup "Scalar" [
    testPropertyNamed "addition is correct" "propAddScalar" propAddScalar
  , testPropertyNamed "negation is correct" "propNegateScalar" propNegateScalar
  , testPropertyNamed "multiplication is correct" "propMulScalar" propMulScalar
  , testPropertyNamed "inverse is correct" "propInverseScalar" propInverseScalar
  , testPropertyNamed "division is correct" "propDivScalar" propDivScalar
  , testPropertyNamed "primitive nth root is correct" "propPrimitiveNthRoot" propPrimitiveNthRoot
  , testPropertyNamed "toWord64 preserve equality" "propToWord64PreservesEq" propToWord64PreservesEq
  , testPropertyNamed "generator ^ (order - 1) = 1" "propGenerator" propGenerator
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


propDivScalar :: Property
propDivScalar = property $ do
  a <- forAll genScalar
  b <- forAll genScalar
  let a' = scalarToInteger a
  case inverseScalar b of
    Just bi -> do
      let bi' = scalarToInteger bi
          c' = (a' * bi') `mod` word64ToInteger order
      scalarToInteger (a `quot` b) === c'
    Nothing -> pure ()


propPrimitiveNthRoot :: Property
propPrimitiveNthRoot = property $ do
  n :: Int <- forAll (integral (Range.linear 1 32))
  let m = 2 ^ n
      r = primitiveNthRoot m
  ((^ m) <$> r) === Just 1
  k :: Word64 <- forAll (integral (Range.linear 1 (m-1)))
  ((^ k) <$> r) /== Just 1


propToWord64PreservesEq :: Property
propToWord64PreservesEq = property $ do
  a <- forAll genScalar
  b <- forAll genScalar
  (a == b) === (toWord64 a == toWord64 b)


propGenerator :: Property
propGenerator = property $
  (generator ^ (order - 1)) === 1
