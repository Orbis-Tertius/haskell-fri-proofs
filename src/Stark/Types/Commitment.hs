module Stark.Types.Commitment ( Commitment (Commitment, unCommitment) ) where


import Stark.Types.MerkleHash (MerkleHash)


newtype Commitment = Commitment { unCommitment :: MerkleHash }
