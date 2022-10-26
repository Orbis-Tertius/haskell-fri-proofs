{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Spec.Stark.FriSpec (testFri, propCompleteness)
import Spec.Stark.MerkleTreeSpec (testMerkleTree)
import Spec.Stark.ScalarSpec (testScalar)
import Spec.Stark.UnivariatePolynomialSpec (testUnivariatePolynomial)
import Test.Tasty
  ( TestTree,
    defaultMain,
    testGroup,
  )
import Hedgehog (recheckAt, Seed (Seed))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testFri,
      testMerkleTree,
      testScalar,
      testUnivariatePolynomial
    ]
