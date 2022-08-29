{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Stark.Types.AuthPath ( AuthPath (AuthPath, unAuthPath) ) where


import Codec.Serialise (Serialise)

import Stark.Types.Commitment (Commitment)


newtype AuthPath = AuthPath { unAuthPath :: [Commitment] }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, Serialise)
