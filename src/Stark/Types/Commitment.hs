{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Stark.Types.Commitment ( Commitment (Commitment, unCommitment) ) where


import Codec.Serialise (Serialise)

import Stark.Types.BinaryTree (BinaryTree)
import Stark.Types.MerkleHash (MerkleHash)


newtype Commitment = Commitment { unCommitment :: BinaryTree MerkleHash }
  deriving (Eq, Serialise, Show)
