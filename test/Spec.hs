module Main
  ( main,
  )
where

import Spec.Stark.FriSpec (testFri)
import Spec.Stark.MerkleTreeSpec (testMerkleTree)
<<<<<<< HEAD
import Test.Tasty (TestTree, defaultMain, testGroup)
=======
import Spec.Stark.ScalarSpec (testScalar)
import Spec.Stark.UnivariatePolynomialSpec (testUnivariatePolynomial)
import Test.Tasty
  ( TestTree,
    defaultMain,
    testGroup,
  )
>>>>>>> master

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testFri,
<<<<<<< HEAD
      testMerkleTree
=======
      testMerkleTree,
      testScalar,
      testUnivariatePolynomial
>>>>>>> master
    ]
