{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Stark.Types.CapCommitment ( CapCommitment (CapCommitment, unCapCommitment) ) where


import           Codec.Serialise        (Serialise)

import           Data.Kind              (Type)
import           Stark.Types.BinaryTree (BinaryTree)
import           Stark.Types.Commitment (Commitment)

type CapCommitment :: Type
newtype CapCommitment = CapCommitment { unCapCommitment :: BinaryTree Commitment }
  deriving stock (Eq, Show)
  deriving newtype Serialise
