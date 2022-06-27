module Spec.Stark.UnivariatePolynomialSpec ( spec ) where


import Control.Monad (forM_)
import Data.List (nub)

import Spec.Gen (genScalar)
import Spec.Prelude
import Stark.UnivariatePolynomial (areColinear, evaluate, interpolate)


spec :: Spec
spec = describe "UnivariatePolynomial" $
  modifyMaxSize (const 8) $ do
    interpolateSpec
    areColinearSpec


interpolateSpec :: Spec
interpolateSpec = describe "interpolate" $
  it "evaluates to the correct values" $
    forAll (zip <$> (nub <$> listOf genScalar) <*> listOf genScalar) $ \points ->
      let p = interpolate points
      in forM_ points (\(x,y) -> evaluate p x `shouldBe` y)


areColinearSpec :: Spec
areColinearSpec = describe "areColinear" $ do
  it "recognizes colinear points" $
    forAll ((,,) <$> genScalar <*> genScalar <*> listOf1 genScalar) $ \(m, b, xs) ->
      let y x = m * x + b
          ys = y <$> xs
      in zip xs ys `shouldSatisfy` areColinear

  it "rejects non-colinear point sets" $
     forAll ((,,,) <$> genScalar <*> genScalar <*> genScalar
                   <*> ((:) <$> genScalar <*> listOf1 genScalar))
       $ \(m, b, x', xs) ->
         let y x = m * x + b
             ys = y <$> xs
         in (x', y x' + 1) : zip xs ys `shouldNotSatisfy` areColinear
   
