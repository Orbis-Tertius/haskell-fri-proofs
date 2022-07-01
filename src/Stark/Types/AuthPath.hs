{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Stark.Types.AuthPath ( AuthPath (AuthPath, unAuthPath) ) where


import Codec.Serialise (Serialise)

import Stark.Types.Commitment (Commitment)


newtype AuthPath = AuthPath { unAuthPath :: [Commitment] }
  deriving (Eq, Semigroup, Monoid, Serialise, Show)
