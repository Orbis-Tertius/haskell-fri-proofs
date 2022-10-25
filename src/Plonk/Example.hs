{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin -Wno-unused-imports #-}

module Plonk.Example
  (
  )
where

-- exampleCircuit,
-- exampleCS,
-- exampleGC,
--   , exampleSomething

-- import Data.Functor.Compose (Compose (Compose))
-- import Data.Functor.Identity (Identity (Identity, runIdentity))
-- import Data.Kind (Type)
-- import Data.Maybe (fromMaybe)
-- import Data.Vinyl.TypeLevel (Nat (S, Z))
-- import Math.Algebra.Polynomial.FreeModule (singleton)
-- import Math.Algebra.Polynomial.Monomial.Generic (singletonMonom)
-- import qualified Math.Algebra.Polynomial.Multivariate.Generic as Multi
-- import Plonk.Arithmetization
--   ( circuitWithDataToPolys,
--     combineCircuitPolys,
--     divUniPoly,
--     getZerofier,
--   )
-- import Plonk.Transcript
--   ( CommitmentTo (MkCommitmentTo),
--     Transcript,
--     pCommitmentMessage,
--     qCommitmentMessage,
--   )
-- import Plonk.Types.Circuit
--   ( Challenge (unChallenge),
--     Z2 (One, Zero),
--     CircuitM (CircuitM, shape),
--     CircuitShape (CNil, (:&)),
--     ColIndex (ColIndex),
--     ColType (MkCol),
--     DegreeBound,
--     Domain (Domain),
--     EN (EqCon, NEqCon),
--     Entry,
--     FAI (Advice, Fixed, Instance),
--     GateConstraint (MkGateConstraint),
--     HasData (WithData),
--     RelativeCellRef (MkRelativeCellRef),
--     RelativeRowIndex (RelativeRowIndex),
--   )
-- import Plonk.Types.Fin (Fin (FS, FZ))
-- import Polysemy (Member, Sem)
-- import Polysemy.Error (Error, throw)
-- import Stark.Fri (commitCodeword, getCodeword)
-- import Stark.Fri.Types (Codeword, DomainLength (DomainLength), ExpansionFactor (ExpansionFactor), FriConfiguration (FriConfiguration), NumColinearityTests (NumColinearityTests), Offset (Offset), Omega (Omega))
-- import Stark.Types.CapCommitment (CapCommitment)
-- import Stark.Types.CapLength (CapLength (CapLength))
-- import Stark.Types.FiatShamir
--   ( IOP,
--     respond,
--     sampleChallenge,
--   )
-- import Stark.Types.Scalar (Scalar, generator, primitiveNthRoot)
-- import Stark.Types.UnivariatePolynomial (UnivariatePolynomial)
-- import Stark.UnivariatePolynomial (constant, evaluate, linear)
--
-- type MyCols :: [ColType]
-- type MyCols = '[ 'MkCol 'Instance 'EqCon, 'MkCol 'Advice 'NEqCon, 'MkCol 'Fixed 'EqCon, 'MkCol 'Fixed 'EqCon]

-- type MyCircuitShape :: DegreeBound -> Type
-- type MyCircuitShape d = CircuitShape [] MyCols 'WithData d Scalar

-- exampleCS :: MyCircuitShape d
-- exampleCS =
--   Compose (Identity One :- Identity Zero :- Identity One :- Nil)
--     :& Compose (Identity Zero :- Identity One :- Identity Zero :- Nil)
--     :& Compose (Identity One :- Identity Zero :- Identity Zero :- Nil)
--     :& Compose (Identity One :- Identity Zero :- Identity Zero :- Nil)
--     :& CNil

-- type N3 :: Nat
-- type N3 = 'S ('S ('S 'Z))

-- exampleGC :: [GateConstraint N4 N4 Scalar]
-- exampleGC = [MkGateConstraint $ Multi.Poly (singleton (singletonMonom (MkRelativeCellRef (RelativeRowIndex 0) (ColIndex FZ)) 1) 1)]
--
-- type MyCircuitM :: Type
-- type MyCircuitM = CircuitM [] MyCols 'WithData N4 Scalar

-- type MyCircuitU :: Type
-- type MyCircuitU = CircuitM UnivariatePolynomial MyCols 'WithData N4 Scalar

-- exampleCircuit :: MyCircuitM
-- exampleCircuit = CircuitM exampleCS exampleGC

-- exampleSomething
--   :: Member (IOP (Challenge Scalar) (Transcript Scalar)) r
--   => Member (Error String) r
--   => Sem r ()
-- exampleSomething = do
--   let d :: Domain N4 Scalar
--       d = Domain (fromInteger @Scalar . toInteger)
--
--       z :: UnivariatePolynomial Scalar
--       z = getZerofier d
--
--       y :: MyCircuitU
--       y = circuitWithDataToPolys d exampleCircuit
--
--       domainLength :: Int
--       domainLength = 64
--
--       capLength :: CapLength
--       capLength = CapLength 1
--
--       friConfig :: FriConfiguration
--       friConfig =
--         FriConfiguration
--         (Offset generator)
--         (Omega . fromMaybe (error "could not find omega")
--           $ primitiveNthRoot (fromIntegral domainLength))
--         (DomainLength domainLength)
--         (ExpansionFactor 2)
--         (NumColinearityTests 4)
--         capLength
--
--   alpha <- sampleChallenge
--
--   let p = combineCircuitPolys d y alpha
--
--   case p `divUniPoly` z of
--     Just q -> do
--       let pc :: Codeword
--           pc = getCodeword friConfig p
--           qc :: Codeword
--           qc = getCodeword friConfig q
--           pcc :: CapCommitment
--           pcc = commitCodeword capLength pc
--           qcc :: CapCommitment
--           qcc = commitCodeword capLength qc
--
--       appendToTranscript (pCommitmentMessage (MkCommitmentTo pcc))
--       appendToTranscript (qCommitmentMessage (MkCommitmentTo qcc))
--
--       zeta <- sampleChallenge
--
--       let p0 :: UnivariatePolynomial (Entry 'WithData 'Instance Scalar)
--           p1 :: UnivariatePolynomial (Entry 'WithData 'Advice Scalar)
--           p2 :: UnivariatePolynomial (Entry 'WithData 'Fixed Scalar)
--           p3 :: UnivariatePolynomial (Entry 'WithData 'Fixed Scalar)
--           (Compose p0 :& Compose p1 :& Compose p2 :& Compose p3 :& CNil) =
--             shape y
--
--           colPoly :: Fin N3 -> UnivariatePolynomial Scalar
--           colPoly =
--             \case
--               FZ -> runIdentity <$> p0
--               FS FZ -> runIdentity <$> p1
--               FS (FS FZ) -> runIdentity <$> p2
--               FS (FS (FS FZ)) -> runIdentity <$> p3
--
--           eval :: Fin N3 -> Scalar
--           eval i = evaluate (colPoly i) (unChallenge zeta)
--
--           pZeta :: Scalar
--           pZeta = evaluate p (unChallenge zeta)
--
--           pointEvalQuotient :: Fin N3 -> Maybe (UnivariatePolynomial Scalar)
--           pointEvalQuotient i =
--             (colPoly i - constant (eval i))
--               `divUniPoly` (linear 1 - constant (unChallenge zeta))
--
--       pointEvalQuotients :: [UnivariatePolynomial Scalar]
--         <- maybe (throw "point eval quotient is not perfect") pure
--            $ mapM pointEvalQuotient [FZ, FS FZ, FS (FS FZ), FS (FS (FS (FZ)))]
--
--       pEvalQuotient :: UnivariatePolynomial Scalar
--         <- maybe (throw "p eval quotient is not perfect") pure
--            $ (p - constant pZeta) `divUniPoly`
--              (linear 1 - constant (unChallenge zeta))
--
--       let friInputPoly :: UnivariatePolynomial Scalar
--           friInputPoly = pEvalQuotient
--               + sum [ constant (unChallenge alpha ^ n) * q'
--                     | (n, q') <- [1..] `zip` pointEvalQuotients ]
--
--       -- TODO: run FRI on friInputPoly
--
--       pure ()
--     Nothing -> pure ()
