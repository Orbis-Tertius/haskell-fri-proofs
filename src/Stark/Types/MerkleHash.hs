module Stark.Types.MerkleHash ( MerkleHash (MerkleHash, unMerkleHash) ) where


import Data.ByteString (ByteString)


newtype MerkleHash = MerkleHash { unMerkleHash :: ByteString }
