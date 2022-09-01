{-# LANGUAGE UndecidableInstances #-}


module Plonk.Example
  ( exampleCircuit
  , exampleCS
  , exampleGC
  , exampleChallenge
  ) where


import           Data.Functor.Compose                         (Compose (Compose))
import           Data.Functor.Identity                        (Identity (Identity))
import           Data.Vinyl.TypeLevel                         (Nat (S, Z))
import           Math.Algebra.Polynomial.FreeModule           (singleton)
import           Math.Algebra.Polynomial.Monomial.Generic     (singletonMonom)
import qualified Math.Algebra.Polynomial.Multivariate.Generic as Multi
import           Plonk.Types.Circuit                          (Challenge (Challenge),
                                                               CircuitM (CircuitM),
                                                               CircuitShape (CNil, (:&)),
                                                               ColIndex (ColIndex),
                                                               ColType (MkCol),
                                                               EN (EqCon, NEqCon),
                                                               FAI (Advice, Fixed, Instance),
                                                               GateConstraint (MkGateConstraint),
                                                               HasData (WithData),
                                                               RelativeCellRef (MkRelativeCellRef),
                                                               RelativeRowIndex (RelativeRowIndex))
import           Plonk.Types.Fin                              (Fin (FZ))
import           Plonk.Types.Vect                             (Vect (Nil, (:-)))
import           Plonk.Types.Z2                               (Z2 (One, Zero))

type MyC :: [ColType]
type MyC = '[ 'MkCol 'Instance 'EqCon, 'MkCol 'Advice 'NEqCon, 'MkCol 'Fixed 'EqCon, 'MkCol 'Fixed 'EqCon ]

exampleCS :: CircuitShape (Vect ('S ('S ('S 'Z)))) MyC 'WithData d Z2
exampleCS = Compose (Identity One  :- Identity Zero :- Identity One :- Nil)
       :& Compose (Identity Zero :- Identity One  :- Identity Zero :- Nil)
       :& Compose (Identity One  :- Identity Zero :- Identity Zero :- Nil)
       :& Compose (Identity One  :- Identity Zero :- Identity Zero :- Nil)
       :& CNil


exampleGC :: [GateConstraint ('S ('S ('S ('S 'Z)))) ('S ('S 'Z)) Z2]
exampleGC = [MkGateConstraint $ Multi.Poly (singleton (singletonMonom (MkRelativeCellRef (RelativeRowIndex 0) (ColIndex FZ)) 1) One)]

exampleCircuit :: CircuitM (Vect ('S ('S ('S 'Z)))) MyC 'WithData ('S ('S 'Z)) Z2
exampleCircuit = CircuitM exampleCS exampleGC

exampleChallenge :: Challenge Z2
exampleChallenge = Challenge Zero
