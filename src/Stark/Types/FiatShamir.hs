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
  Type -> -- t: transcript
  Type -> -- d: response
  (Type -> Type) -> -- m: monad
  Type -> -- a: result
  Type
data IOP c t d m a where
  SampleChallenge :: IOP c t d m c
  Respond :: r -> IOP c t d m ()

makeSem ''IOP

proverFiatShamir ::
  Sampleable c =>
  Serialise d =>
  Members '[State t] effs =>
  (d -> t -> t) ->
  Sem (IOP c d ': effs) a ->
  Sem effs a
proverFiatShamir applyDelta =
  interpret $
    \case
      Respond r -> do
        put . (r `applyDelta`) =<< get
      SampleChallenge -> do
        transcript <- get
        pure (sample (BSL.toStrict (serialise transcript)))

newtype DeltaIndex d = DeltaIndex { unDeltaIndex :: Int }

verifierFiatShamir ::
  Eq r =>
  Sampleable c =>
  Serialise d =>
  Show d =>
  Members '[Error ErrorMessage] effs =>
  (t -> DeltaIndex d -> d) ->
  Sem (IOP c r ': effs) a ->
  Sem effs a
verifierFiatShamir extractDeltas =
  interpret $
    \case
      Respond r -> do
        TranscriptPartition (consumed, unconsumed) <- get
        case unconsumed of
          (r' : rest) ->
            if r == r'
              then put (TranscriptPartition (consumed <> [r], rest))
              else
                throw . ErrorMessage $
                  "verifierFiatShamir: Respond: responses are out of order: expected "
                    <> show r
                    <> " but got "
                    <> show r'
          [] -> throw (ErrorMessage "verifierFiatShamir: Respond: premature end of transcript")
      SampleChallenge -> do
        TranscriptPartition (consumed, _) <- get
        pure (sample (BSL.toStrict (serialise consumed)))
