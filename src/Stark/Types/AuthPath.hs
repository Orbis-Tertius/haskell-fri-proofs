module Stark.Types.AuthPath (AuthPath (AuthPath, unAuthPath)) where

import Codec.Serialise (Serialise)
import Data.Kind (Type)
import Stark.Types.Commitment (Commitment)

type AuthPath :: Type
-- TODO: reduce to one commitment
-- Explanation: the auth path elements each contain
-- the left and right hash and the merged hash
-- (in that order).
newtype AuthPath = AuthPath {unAuthPath :: [(Commitment, Commitment, Commitment)]}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, Serialise)
