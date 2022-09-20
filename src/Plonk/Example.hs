{-# LANGUAGE UndecidableInstances #-}


module Plonk.Example
  ( exampleCircuit
  , exampleCS
  , exampleGC
  , exampleChallenge
  , exampleSomething
  ) where


import           Control.Applicative                          ((<|>))
import           Data.Functor.Compose                         (Compose (Compose))
import           Data.Functor.Identity                        (Identity (Identity))
import           Data.Kind                                    (Type)
import           Data.Map                                     (Map)
import qualified Data.Map                                     as Map
import           Data.Monoid.Generic                          (GenericMonoid (GenericMonoid),
                                                               GenericSemigroup (GenericSemigroup))
import           Data.Vinyl.TypeLevel                         (Nat (S, Z))
import           GHC.Generics                                 (Generic)
import           Math.Algebra.Polynomial.FreeModule           (singleton)
import           Math.Algebra.Polynomial.Monomial.Generic     (singletonMonom)
import qualified Math.Algebra.Polynomial.Multivariate.Generic as Multi
import           Plonk.Arithmetization                        (circuitWithDataToPolys,
                                                               combineCircuitPolys)
import           Plonk.Types.Circuit                          (Challenge (Challenge),
                                                               CircuitM (CircuitM),
                                                               CircuitShape (CNil, (:&)),
                                                               ColIndex (ColIndex),
                                                               ColType (MkCol),
                                                               DegreeBound,
                                                               Domain (Domain),
                                                               EN (EqCon, NEqCon),
                                                               FAI (Advice, Fixed, Instance),
                                                               GateConstraint (MkGateConstraint),
                                                               HasData (WithData),
                                                               RelativeCellRef (MkRelativeCellRef),
                                                               RelativeRowIndex (RelativeRowIndex))
import           Plonk.Types.Fin                              (Fin (FZ))
import           Plonk.Types.Vect                             (Vect (Nil, (:-)))
import           Plonk.Types.Z2                               (Z2 (One, Zero))
import           Polysemy                                     (Member, Sem)
import           Stark.Types.AuthPath                         (AuthPath)
import           Stark.Types.Commitment                       (Commitment)
import           Stark.Types.FiatShamir                       (IOP)
import           Stark.Types.Index                            (Index)
import           Stark.Types.UnivariatePolynomial             (UnivariatePolynomial)

type MyCols :: [ColType]
type MyCols = '[ 'MkCol 'Instance 'EqCon, 'MkCol 'Advice 'NEqCon, 'MkCol 'Fixed 'EqCon, 'MkCol 'Fixed 'EqCon ]

type MyCircuitShape :: DegreeBound -> Type
type MyCircuitShape d = CircuitShape (Vect ('S ('S ('S 'Z)))) MyCols 'WithData d Z2

exampleCS :: MyCircuitShape d
exampleCS = Compose (Identity One  :- Identity Zero :- Identity One :- Nil)
       :& Compose (Identity Zero :- Identity One  :- Identity Zero :- Nil)
       :& Compose (Identity One  :- Identity Zero :- Identity Zero :- Nil)
       :& Compose (Identity One  :- Identity Zero :- Identity Zero :- Nil)
       :& CNil


exampleGC :: [GateConstraint ('S ('S ('S ('S 'Z)))) ('S ('S 'Z)) Z2]
exampleGC = [MkGateConstraint $ Multi.Poly (singleton (singletonMonom (MkRelativeCellRef (RelativeRowIndex 0) (ColIndex FZ)) 1) One)]

type MyCircuitM :: Type
type MyCircuitM = CircuitM (Vect ('S ('S ('S 'Z)))) MyCols 'WithData ('S ('S 'Z)) Z2

type MyCircuitU :: Type
type MyCircuitU = CircuitM UnivariatePolynomial MyCols 'WithData ('S ('S 'Z)) Z2

exampleCircuit :: MyCircuitM
exampleCircuit = CircuitM exampleCS exampleGC

exampleChallenge :: Challenge Z2
exampleChallenge = Challenge Zero

type CommitmentToP :: Type
newtype CommitmentToP = CommitmentToP Commitment

type CommitmentToQ :: Type
newtype CommitmentToQ = CommitmentToQ Commitment

type Commitments :: Type
data Commitments =
  Commitments
  (Maybe CommitmentToP)
  (Maybe CommitmentToQ)

instance Semigroup Commitments where
  (Commitments a b) <> (Commitments c d) = Commitments (a <|> c) (b <|> d)

instance Monoid Commitments where
  mempty = Commitments Nothing Nothing

type Openings :: Type
newtype Openings = Openings (Map Index (Z2, AuthPath))
  deriving newtype (Semigroup, Monoid)

type Transcript :: Type
data Transcript =
  Transcript
  { challenges  :: [Challenge Z2]
  , commitments :: Commitments
  , openings    :: Openings
  }
  deriving stock Generic
  deriving Semigroup via GenericSemigroup Transcript
  deriving Monoid via GenericMonoid Transcript

challengeMessage :: Challenge Z2 -> Transcript
challengeMessage c = Transcript [c] mempty mempty

qCommitmentMessage :: CommitmentToQ -> Transcript
qCommitmentMessage qc = Transcript [] (Commitments Nothing (Just qc)) mempty

pCommitmentMessage :: CommitmentToP -> Transcript
pCommitmentMessage pc = Transcript [] (Commitments (Just pc) Nothing) mempty

openingMessage :: Index -> AuthPath -> Z2 -> Transcript
openingMessage i a x = Transcript [] mempty (Openings (Map.singleton i (x, a)))

exampleSomething :: Member (IOP (Challenge Z2) Transcript) r
  => Sem r (UnivariatePolynomial Z2)
exampleSomething =
  let
    x :: Domain d Z2
    x = Domain (fromInteger @Z2 . toInteger)

    y :: MyCircuitU
    y = circuitWithDataToPolys x exampleCircuit

  in pure (combineCircuitPolys x y exampleChallenge)
