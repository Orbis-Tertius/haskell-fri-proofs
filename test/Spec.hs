module Main
  ( main
  ) where

import Spec.Stark.FriSpec (testFri)

main :: IO ()
main = withUtf8 $ defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ testFri
                          ]
