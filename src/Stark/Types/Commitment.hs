{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Stark.Types.Commitment ( Commitment (Commitment, unCommitment) ) where


import Codec.Serialise (Serialise)

import Stark.Types.MerkleHash (MerkleHash)


newtype Commitment = Commitment { unCommitment :: MerkleHash }
  deriving (Eq, Serialise, Show)
