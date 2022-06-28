{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Stark.Types.Leaf ( Leaf (Leaf, unLeaf) ) where


import Codec.Serialise (Serialise)

import Stark.Types.MerkleHash (MerkleHash)


newtype Leaf = Leaf { unLeaf :: MerkleHash }
  deriving (Serialise, Show)
