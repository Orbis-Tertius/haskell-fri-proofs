{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}


module Stark.Types.FiatShamir 
  ( IOP
  , appendToTranscript
  , fiatShamir
  , sampleChallenge
  , Sampleable(sample)
  ) where


import           Codec.Serialise      (Serialise, serialise)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Kind            (Constraint, Type)
import           Polysemy             (Member, Members, Sem, interpret, makeSem)
import           Polysemy.State       (State, get, put)

import           Stark.Hash           (hash)

type Sampleable :: Type -> Constraint
class Sampleable a where
  sample :: BS.ByteString -> a


type IOP :: Type -> Type -> (Type -> Type) -> Type -> Type
data IOP c t m a where
  AppendToTranscript :: t -> IOP c t m ()
  SampleChallenge :: IOP c t m c

makeSem ''IOP

fiatShamir
  :: Sampleable c
  => Serialise t
  => Members '[State t] r
  => Monoid t
  => Sem (IOP c t ': r) a -> Sem r a
fiatShamir = interpret $
  \case
    AppendToTranscript t -> do
     put . (<> t) =<< get
    SampleChallenge -> do
     x :: q <- get
     pure (sample (BSL.toStrict (serialise x)))

