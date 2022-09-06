{-# LANGUAGE UndecidableInstances #-}


module Plonk.Example
  ( exampleCircuit
  , exampleCS
  , exampleGC
  , exampleChallenge
  , exampleSomething
  ) where


import           Data.Functor.Compose                         (Compose (Compose))
import           Data.Functor.Identity                        (Identity (Identity))
import           Data.Kind                                    (Type)
import           Data.Vinyl.TypeLevel                         (Nat (S, Z))
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
                                                               DomainGenerator (DomainGenerator),
                                                               EN (EqCon, NEqCon),
                                                               FAI (Advice, Fixed, Instance),
                                                               GateConstraint (MkGateConstraint),
                                                               HasData (WithData),
                                                               RelativeCellRef (MkRelativeCellRef),
                                                               RelativeRowIndex (RelativeRowIndex))
import           Plonk.Types.Fin                              (Fin (FZ))
import           Plonk.Types.Vect                             (Vect (Nil, (:-)))
import           Plonk.Types.Z2                               (Z2 (One, Zero))
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

exampleSomething :: Maybe (UnivariatePolynomial Z2)
exampleSomething =
  let x = DomainGenerator Zero
      y :: Maybe MyCircuitU
      y = circuitWithDataToPolys (Domain x) exampleCircuit
  in case y of
       Nothing -> Nothing
       Just y' -> Just $ combineCircuitPolys x y' exampleChallenge
