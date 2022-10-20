module Stark.Types.Commitment (Commitment (Commitment, unCommitment)) where

import Codec.Serialise (Serialise)
import Data.Kind (Type)
import Stark.Types.MerkleHash (MerkleHash)

type Commitment :: Type
newtype Commitment = Commitment {unCommitment :: MerkleHash}
  deriving stock (Eq, Show)
  deriving newtype (Serialise)
