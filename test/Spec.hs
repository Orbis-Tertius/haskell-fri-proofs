module Main
  ( main
  ) where

import Spec.Stark.FriSpec (testFri)
import Spec.Stark.MerkleTreeSpec (testMerkleTree)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ testFri
                          , testMerkleTree
                          ]
