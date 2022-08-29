module Stark.Types.MerkleHash ( MerkleHash (MerkleHash, unMerkleHash) ) where


import           Codec.Serialise (Serialise)
import Data.Kind (Type)
import           Data.ByteString (ByteString)


type MerkleHash :: Type
newtype MerkleHash = MerkleHash { unMerkleHash :: ByteString }
  deriving stock (Eq, Show)
  deriving newtype Serialise
