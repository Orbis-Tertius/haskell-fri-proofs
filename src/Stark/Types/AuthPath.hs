{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Stark.Types.AuthPath ( AuthPath (AuthPath, unAuthPath) ) where


import Stark.Types.MerkleHash (MerkleHash)


newtype AuthPath = AuthPath { unAuthPath :: [MerkleHash] }
  deriving (Semigroup, Monoid)
