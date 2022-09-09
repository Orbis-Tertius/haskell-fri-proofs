{-# LANGUAGE TypeFamilies #-}


module Stark.Types.FiatShamir (FiatShamir (..)) where


import Data.Kind (Type)


type IOP :: Type -> (Type -> Type) -> Type -> Type
data IOP c t m a where
  AppendToTranscript :: t -> m ()
  SampleChallenge :: m c

makeSem ''IOP

-- appendToTranscript :: Member '[IOP c t] r => t -> Sem r () 

program :: Member (IOP c t) r => Sem r a
program = do
  appendToTranscript _
  x <- sampleChallenge
  appendToTranscript _
  x <- sampleChallenge
  appendToTranscript _


fiatShamir :: Sem '[IOP c t] a -> Sem '[State q] a
fiatShamir = interpret \case
  AppendToTranscript t -> do
   put monoidally t
  SampleChallenge -> 
   x <- get
   hash

newtype Proof a = Proof { unProof :: ByteString }


fiatShamir :: IOP a -> Proof a


class Monad m => FiatShamir m where
  type TranscriptElement m :: Type
  type Challenge m :: Type
  appendToTranscript :: TranscriptElement m -> m ()
  sampleChallenge :: m (Challenge m)
