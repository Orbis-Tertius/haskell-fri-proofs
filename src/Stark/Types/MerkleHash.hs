module Stark.Types.MerkleHash (MerkleHash (MerkleHash, unMerkleHash)) where

import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Kind (Type)

type MerkleHash :: Type
newtype MerkleHash = MerkleHash {unMerkleHash :: ByteString}
  deriving stock (Eq, Show)
  deriving newtype (Serialise)
