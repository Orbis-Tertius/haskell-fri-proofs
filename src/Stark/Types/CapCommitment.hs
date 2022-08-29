{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Stark.Types.CapCommitment ( CapCommitment (CapCommitment, unCapCommitment) ) where


import           Codec.Serialise        (Serialise)

import           Stark.Types.BinaryTree (BinaryTree)
import           Stark.Types.Commitment (Commitment)


newtype CapCommitment = CapCommitment { unCapCommitment :: BinaryTree Commitment }
  deriving stock (Eq, Show)
  deriving newtype Serialise
