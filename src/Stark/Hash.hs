module Stark.Hash (hash) where

import qualified Crypto.Hash.BLAKE2.BLAKE2b as BLAKE2b
import Data.ByteString (ByteString)

hash :: ByteString -> ByteString
hash = BLAKE2b.hash 64 mempty
