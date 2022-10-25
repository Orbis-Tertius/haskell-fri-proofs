{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Stark.Types.FiatShamir
  ( IOP,
    ErrorMessage (ErrorMessage, unErrorMessage),
    Transcript (Transcript, unTranscript),
    TranscriptPartition (TranscriptPartition, unTranscriptPartition),
    sampleChallenge,
    respond,
    proverFiatShamir,
    verifierFiatShamir,
    Sampleable (sample),
  )
where

import Codec.Serialise (Serialise, serialise)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Kind (Constraint, Type)
import Data.String (IsString)
import GHC.Generics (Generic)
import Polysemy (Members, Sem, interpret, makeSem)
import Polysemy.Error (Error, throw)
import Polysemy.State (State, get, put)
import Stark.Prelude ()

type Sampleable :: Type -> Constraint
class Sampleable a where
  sample :: BS.ByteString -> a

type ErrorMessage :: Type
newtype ErrorMessage = ErrorMessage {unErrorMessage :: String}
  deriving newtype (IsString, Eq, Show)

type IOP ::
  Type -> -- c: challenge
  Type -> -- r: response
  (Type -> Type) -> -- m: monad
  Type -> -- a: result
  Type
data IOP c r m a where
  SampleChallenge :: IOP c r m c
  Respond :: r -> IOP c r m ()

type Transcript :: Type -> Type
newtype Transcript r = Transcript {unTranscript :: [r]}
  deriving newtype (Eq, Semigroup, Monoid, Serialise)
  deriving stock (Generic, Show)

makeSem ''IOP

proverFiatShamir ::
  Sampleable c =>
  Serialise r =>
  Members '[State (Transcript r)] effs =>
  Sem (IOP c r ': effs) a ->
  Sem effs a
proverFiatShamir =
  interpret $
    \case
      Respond r -> do
        put . (<> Transcript [r]) =<< get
      SampleChallenge -> do
        transcript <- get
        pure (sample (BSL.toStrict (serialise transcript)))

type TranscriptPartition :: Type -> Type
newtype TranscriptPartition r = TranscriptPartition
  {unTranscriptPartition :: (Transcript r, Transcript r)}

verifierFiatShamir ::
  Eq r =>
  Sampleable c =>
  Serialise r =>
  Show r =>
  Members '[Error ErrorMessage] effs =>
  Members '[State (TranscriptPartition r)] effs =>
  Sem (IOP c r ': effs) a ->
  Sem effs a
verifierFiatShamir =
  interpret $
    \case
      Respond r -> do
        TranscriptPartition (consumed, unconsumed) <- get
        case unconsumed of
          Transcript (r' : rest) ->
            if r == r'
            then put (TranscriptPartition (consumed <> Transcript [r], Transcript rest))
            else throw . ErrorMessage
              $ "verifierFiatShamir: Respond: responses are out of order: expected "
             <> show r <> " but got " <> show r'
          Transcript [] -> throw (ErrorMessage "verifierFiatShamir: Respond: premature end of transcript")
      SampleChallenge -> do
        TranscriptPartition (consumed, _) <- get
        pure (sample (BSL.toStrict (serialise consumed)))
