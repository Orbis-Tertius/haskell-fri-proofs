module Stark.MerkleTree
  ( dataToLeaf
  , commit
  , commit_
  ) where


import Codec.Serialise (Serialise, serialise)
import qualified Crypto.Hash.BLAKE2.BLAKE2b as BLAKE2b
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL

import Stark.Types.BinaryTree (BinaryTree (IsLeaf, IsNode))
import Stark.Types.Commitment (Commitment (Commitment))
import Stark.Types.Leaf (Leaf (Leaf))
import Stark.Types.MerkleHash (MerkleHash (MerkleHash))


hash :: ByteString -> ByteString
hash = BLAKE2b.hash 64 mempty


hashData :: Serialise a => a -> MerkleHash
hashData = MerkleHash . hash . BSL.toStrict . serialise


dataToLeaf :: Serialise a => a -> Leaf
dataToLeaf = Leaf . hashData


commit :: Serialise a => BinaryTree a -> Commitment
commit = commit_ . fmap hashData


commit_ :: BinaryTree MerkleHash -> Commitment
commit_ (IsLeaf x) = Commitment x
commit_ (IsNode x y) = Commitment (hashData (commit x, commit y))
