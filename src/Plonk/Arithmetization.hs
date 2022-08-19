{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}


module Plonk.Arithmetization
  ( columnVectorToPoly
  , circuitWithDataToPolys
  , circTraverse
  , circShapeTraverse
  , combineCircuitPolys
  , getZerofier
  , divUniPoly
  ) where


import Plonk.Types.Circuit
import Data.Functor.Identity
import Data.Functor.Compose
import Stark.Types.Scalar (Scalar)
import Stark.Types.UnivariatePolynomial (UnivariatePolynomial)


columnVectorToPoly
  :: Domain n a
  -> Vect n a
  -> Maybe (UnivariatePolynomial a)
columnVectorToPoly = todo -- this can be done with an FFT (Faez)


circuitWithDataToPolys
  :: Domain m a
  -> Circuit ps 'WithData m d a
  -> Maybe (Circuit' UnivariatePolynomial ps 'WithData d a)
circuitWithDataToPolys dom = circTraverse (columnVectorToPoly dom)


circTraverse :: Applicative g
             => Functor f
             => Functor h
             => (h a -> g (f a))
             -> Circuit' h ps 'WithData d a
             -> g (Circuit' f ps 'WithData d a)
circTraverse k (Circuit shape constraints) =
   flip Circuit constraints <$> circShapeTraverse k shape


circShapeTraverse :: Applicative g
                  => Functor f
                  => Functor h
                  => (h a -> g (f a))
                  -> CircuitShape h ps 'WithData d a
                  -> g (CircuitShape f ps 'WithData d a)
circShapeTraverse _ CNil = pure CNil
circShapeTraverse k (x :& xs) =
  (:&) <$> wrapInIdentity k x <*> circShapeTraverse k xs


wrapInIdentity
  :: Functor g
  => Functor f
  => Functor h
  => (h a -> g (f a))
  -> (Compose h Identity) a -> g ((Compose f Identity) a)
wrapInIdentity f (Compose xs) =
  Compose . fmap Identity
    <$> f (runIdentity <$> xs)


plugInDataToGateConstraint
  :: CircuitShape UnivariatePolynomial ps 'WithData d a
  -> GateConstraint (Length ps) d a
  -> Maybe (UnivariatePolynomial a)
plugInDataToGateConstraint = todo


linearlyCombineGatePolys
  :: Challenge a
  -> [UnivariatePolynomial a]
  -> UnivariatePolynomial a
linearlyCombineGatePolys = todo


combineCircuitPolys
  :: Circuit' UnivariatePolynomial ps 'WithData d a
  -> Challenge a
  -> Maybe (UnivariatePolynomial a)
combineCircuitPolys (Circuit shape gates) challenge =
  linearlyCombineGatePolys challenge <$>
    sequence (plugInDataToGateConstraint shape <$> gates)


getZerofier :: Domain n a -> UnivariatePolynomial a
getZerofier = todo -- this can be done with an FFT (Faez)


-- returns the quotient if the denominator
-- perfectly divides the numerator.
divUniPoly :: UnivariatePolynomial Scalar
           -> UnivariatePolynomial Scalar
           -> Maybe (UnivariatePolynomial a)
divUniPoly = todo -- can be done with Euclidean algorithm


-- IOP idea:
--  * Given a root of unity which generates a domain, d
--  * Given a circuit with data, c
--     * It expresses a true statement; its constraints are satisfied
--  * Verifier provides a challenge alpha
--  * Make the polynomial p(x) = combineCircuitPolys c alpha
--  * Make the zerofier z(x) = getZerofier d
--  * Make the quotient q(x) = p(x) `divUniPoly` z(x)
--  * Commit to p(x) and q(x), send commitments to verifier
--  * Verifier provides a challenge zeta
--  * Want to show: p(zeta) = q(zeta) * z(zeta)
--  * So, prover sends openings q(zeta) and p(zeta)
--  * Verifier checks the equation
--  * Repeat until verifier is convinced
--
-- zk-SNARK protocol: apply the Fiat-Shamir transform to the IOP
--  * Faez has been working on abstraction for doing this
--    (applying Fiat-Shamir to an IOP)


todo :: a
todo = todo
