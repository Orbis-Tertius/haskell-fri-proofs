module Stark.MerkleTree
  ( dataToLeaf
  ) where


import Codec.Serialise (Serialise, serialise)
import qualified Crypto.Hash.BLAKE2.BLAKE2b as BLAKE2b
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL

import Stark.Types.Leaf (Leaf (Leaf))
import Stark.Types.MerkleHash (MerkleHash (MerkleHash))


hash :: ByteString -> ByteString
hash = BLAKE2b.hash 64 mempty


dataToLeaf :: Serialise a => a -> Leaf
dataToLeaf = Leaf . MerkleHash . hash . BSL.toStrict . serialise
