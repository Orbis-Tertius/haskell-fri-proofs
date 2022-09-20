module Plonk.Transcript
  ( challengeMessage
  , qCommitmentMessage
  , pCommitmentMessage
  , openingMessage
  , PQ(P, Q)
  , CommitmentTo(..)
  , Commitments(..)
  , Openings(..)
  , Transcript(..)
  ) where

import           Control.Applicative    ((<|>))
import           Data.Kind              (Type)
import           Data.Map               as Map (Map, singleton)
import           Data.Monoid.Generic    (GenericMonoid (GenericMonoid),
                                         GenericSemigroup (GenericSemigroup))
import           GHC.Generics           (Generic)
import           Plonk.Types.Circuit    (Challenge)
import           Stark.Types.AuthPath   (AuthPath)
import           Stark.Types.Commitment (Commitment)
import           Stark.Types.Index      (Index)

type PQ :: Type
data PQ where
  P :: PQ
  Q :: PQ

type CommitmentTo :: PQ -> Type
newtype CommitmentTo pq = MkCommitmentTo Commitment

type Commitments :: Type
data Commitments where
  MkCommitments :: Maybe (CommitmentTo 'P)
                -> Maybe (CommitmentTo 'Q)
                -> Commitments

instance Semigroup Commitments where
  MkCommitments a b <> MkCommitments c d = MkCommitments (a <|> c) (b <|> d)

instance Monoid Commitments where
  mempty = MkCommitments Nothing Nothing

type Openings :: Type -> Type
newtype Openings x = Openings (Map Index (x, AuthPath))
  deriving newtype (Semigroup, Monoid)

type Transcript :: Type -> Type
data Transcript x =
  Transcript
  { challenges  :: [Challenge x]
  , commitments :: Commitments
  , openings    :: Openings x
  }
  deriving stock Generic
  deriving Semigroup via GenericSemigroup (Transcript x)
  deriving Monoid via GenericMonoid (Transcript x)

challengeMessage :: Challenge x -> Transcript x
challengeMessage c = Transcript [c] mempty mempty

qCommitmentMessage :: CommitmentTo 'Q -> Transcript x
qCommitmentMessage qc = Transcript [] (MkCommitments Nothing (Just qc)) mempty

pCommitmentMessage :: CommitmentTo 'P -> Transcript x
pCommitmentMessage pc = Transcript [] (MkCommitments (Just pc) Nothing) mempty

openingMessage :: Index -> AuthPath -> x -> Transcript x
openingMessage i a x = Transcript [] mempty (Openings (Map.singleton i (x, a)))
