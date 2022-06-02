module Stark.Types.Leaf ( Leaf (Leaf, unLeaf) ) where


import Stark.Types.MerkleHash (MerkleHash)


newtype Leaf = Leaf { unLeaf :: MerkleHash }
