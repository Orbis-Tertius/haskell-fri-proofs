module Stark.Types.Commitment ( Commitment (Commitment, unCommitment) ) where


import           Codec.Serialise        (Serialise)

import           Stark.Types.MerkleHash (MerkleHash)


newtype Commitment = Commitment { unCommitment :: MerkleHash }
  deriving stock (Eq, Show)
  deriving newtype Serialise
