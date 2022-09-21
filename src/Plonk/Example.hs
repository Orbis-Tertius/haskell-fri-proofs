{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}


module Plonk.Example
  ( exampleCircuit
  , exampleCS
  , exampleGC
  , exampleSomething
  ) where


import Stark.Fri (getCodeword, commitCodeword)
import Stark.Fri.Types (FriConfiguration (FriConfiguration), Codeword, Offset (Offset), Omega (Omega), DomainLength (DomainLength), ExpansionFactor (ExpansionFactor), NumColinearityTests (NumColinearityTests))
import Stark.FiniteField (primitiveNthRoot, generator)
import Stark.Types.CapCommitment (CapCommitment)
import Stark.Types.CapLength (CapLength (CapLength))
import Data.Maybe (fromMaybe)
import           Data.Functor.Compose                         (Compose (Compose))
import           Data.Functor.Identity                        (Identity (Identity))
import           Data.Kind                                    (Type)
import           Data.Vinyl.TypeLevel                         (Nat (S, Z))
import           Math.Algebra.Polynomial.FreeModule           (singleton)
import           Math.Algebra.Polynomial.Monomial.Generic     (singletonMonom)
import qualified Math.Algebra.Polynomial.Multivariate.Generic as Multi
import           Plonk.Arithmetization                        (circuitWithDataToPolys, divUniPoly,
                                                               combineCircuitPolys, getZerofier)
import           Plonk.Transcript                             (Transcript,
                                                               pCommitmentMessage, qCommitmentMessage, CommitmentTo (MkCommitmentTo))
import           Plonk.Types.Circuit                          (Challenge,
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
import           Polysemy                                     (Member, Sem)
import           Stark.Types.FiatShamir                       (IOP, sampleChallenge,
                                                               appendToTranscript)
import           Stark.Types.UnivariatePolynomial             (UnivariatePolynomial)
import Stark.Types.Scalar (Scalar)

type MyCols :: [ColType]
type MyCols = '[ 'MkCol 'Instance 'EqCon, 'MkCol 'Advice 'NEqCon, 'MkCol 'Fixed 'EqCon, 'MkCol 'Fixed 'EqCon ]

type MyCircuitShape :: DegreeBound -> Type
type MyCircuitShape d = CircuitShape (Vect ('S ('S ('S ('S 'Z))))) MyCols 'WithData d Scalar

exampleCS :: MyCircuitShape d
exampleCS = Compose (Identity 1  :- Identity 0 :- Identity 1 :- Identity 0 :- Nil)
       :& Compose (Identity 1 :- Identity 1  :- Identity 0 :- Identity 0 :- Nil)
       :& Compose (Identity 1  :- Identity 0 :- Identity 0 :- Identity 0 :- Nil)
       :& Compose (Identity 1  :- Identity 1 :- Identity 0 :- Identity 0 :- Nil)
       :& CNil

type N4 :: Nat
type N4 = 'S ('S ('S ('S 'Z)))

exampleGC :: [GateConstraint N4 N4 Scalar]
exampleGC = [MkGateConstraint $ Multi.Poly (singleton (singletonMonom (MkRelativeCellRef (RelativeRowIndex 0) (ColIndex FZ)) 1) 1)]

type MyCircuitM :: Type
type MyCircuitM = CircuitM (Vect N4) MyCols 'WithData N4 Scalar

type MyCircuitU :: Type
type MyCircuitU = CircuitM UnivariatePolynomial MyCols 'WithData N4 Scalar

exampleCircuit :: MyCircuitM
exampleCircuit = CircuitM exampleCS exampleGC

exampleSomething :: Member (IOP (Challenge Scalar) (Transcript Scalar)) r
  => Sem r ()
exampleSomething = do
  let d :: Domain N4 Scalar
      d = Domain (fromInteger @Scalar . toInteger)

      z :: UnivariatePolynomial Scalar
      z = getZerofier d

      y :: MyCircuitU
      y = circuitWithDataToPolys d exampleCircuit

      domainLength :: Int
      domainLength = 64

      capLength :: CapLength
      capLength = CapLength 1

      friConfig :: FriConfiguration
      friConfig =
        FriConfiguration
        (Offset generator)
        (Omega . fromMaybe (error "could not find omega")
          $ primitiveNthRoot (fromIntegral domainLength))
        (DomainLength domainLength)
        (ExpansionFactor 2)
        (NumColinearityTests 4)
        capLength

  -- TODO: commit to column polys

  alpha <- sampleChallenge

  let p = combineCircuitPolys d y alpha
  case p `divUniPoly` z of
    Just q -> do
      let pc :: Codeword
          pc = getCodeword friConfig p
          qc :: Codeword
          qc = getCodeword friConfig q
          pcc :: CapCommitment
          pcc = commitCodeword capLength pc
          qcc :: CapCommitment
          qcc = commitCodeword capLength qc
      appendToTranscript (pCommitmentMessage (MkCommitmentTo pcc))
      appendToTranscript (qCommitmentMessage (MkCommitmentTo qcc))
      _zeta <- sampleChallenge
      pure ()
    Nothing -> pure ()
