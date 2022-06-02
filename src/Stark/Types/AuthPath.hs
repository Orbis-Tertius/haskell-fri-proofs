module Stark.Types.AuthPath ( AuthPath (AuthPath, unAuthPath) ) where


import Stark.Types.BinaryTree (BinaryTree)
import Stark.Types.MerkleHash (MerkleHash)


newtype AuthPath = AuthPath { unAuthPath :: BinaryTree MerkleHash }
