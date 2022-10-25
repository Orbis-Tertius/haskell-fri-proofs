module Stark.Types.AuthPath (AuthPath (AuthPath, unAuthPath)) where

import Codec.Serialise (Serialise)
import Data.Kind (Type)
import Stark.Types.Commitment (Commitment)

type AuthPath :: Type
newtype AuthPath = AuthPath {unAuthPath :: [(Commitment, Commitment)]}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, Serialise)
