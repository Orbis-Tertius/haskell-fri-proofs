{-# LANGUAGE RankNTypes #-}


module Plonk.Arithmetization
  ( columnVectorToPoly
  , circuitWithDataToPolys
  , circTraverse
  , combineCircuitPolys
  , getZerofier
  , divUniPoly
  ) where


import Plonk.Types.Circuit
import Stark.Types.Scalar (Scalar)


columnVectorToPoly
  :: Domain n a
  -> Vect n a
  -> Maybe (UnivariatePolynomial a)
columnVectorToPoly = todo -- this can be done with an FFT (Faez)


circuitWithDataToPolys
  :: Domain m a
  -> CircuitWithData ps m d a
  -> Maybe (CircuitWithData' UnivariatePolynomial ps d a)
circuitWithDataToPolys dom = circTraverse (columnVectorToPoly dom)


circTraverse :: (h a -> g (f a))
             -> CircuitWithData' h ps d a
             -> g (CircuitWithData' f ps d a)
circTraverse = todo


plugInDataToGateConstraint
  :: CircuitShape UnivariatePolynomial ps d a a
  -> GateConstraint (Length ps) d a
  -> Maybe (UnivariatePolynomial a)
plugInDataToGateConstraint = todo


linearlyCombineGatePolys
  :: Challenge a
  -> [UnivariatePolynomial a]
  -> UnivariatePolynomial a
linearlyCombineGatePolys = todo


combineCircuitPolys
  :: CircuitWithData' UnivariatePolynomial ps d a
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
