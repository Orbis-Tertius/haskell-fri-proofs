{-# LANGUAGE LambdaCase #-}
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
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Kind (Constraint, Type)
import Data.String (IsString)
import Polysemy (Members, Sem, interpret, makeSem)
import Polysemy.Error (Error, throw)
import Polysemy.Input (Input, input)
import Polysemy.State (State, get, put)

type Sampleable :: Type -> Constraint
class Sampleable a where
  sample :: BS.ByteString -> a

type ErrorMessage :: Type
newtype ErrorMessage = ErrorMessage {unErrorMessage :: String}
  deriving newtype (IsString)

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
  Sampleable c =>
  Serialise r =>
  Eq r =>
  Members '[Input (Transcript r)] effs =>
  Members '[State (TranscriptPartition r)] effs =>
  Members '[Error ErrorMessage] effs =>
  Sem (IOP c r ': effs) a ->
  Sem effs a
verifierFiatShamir =
  interpret $
    \case
      Respond r -> do
        TranscriptPartition (consumed, unconsumed) <- get
        case consumed of
          Transcript [] -> put . TranscriptPartition . (Transcript mempty,) =<< input
          _ -> pure ()
        case unconsumed of
          Transcript (r' : rest) -> do
            unless
              (r == r')
              (throw "verifierFiatShamir: out of order responses; maybe this proof does not match the statement?")
            put $
              TranscriptPartition
                ( consumed <> Transcript [r'],
                  Transcript rest
                )
          Transcript [] -> throw "verifierFiatShamir: unexpected end of transcript"
      SampleChallenge -> do
        TranscriptPartition (consumed, _) <- get
        pure (sample (BSL.toStrict (serialise consumed)))
