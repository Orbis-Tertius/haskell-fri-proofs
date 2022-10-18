{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}


module Stark.Types.FiatShamir
  ( IOP
  , setup
  , getTranscript
  , appendToTranscript
  , reject
  , sampleChallenge
  , proverFiatShamir
  , verifierFiatShamir
  , Sampleable(sample)
  ) where


import           Codec.Serialise      (Serialise)
import qualified Data.ByteString      as BS
import           Data.Kind            (Constraint, Type)
import           Polysemy             (Members, Sem, makeSem)
import Polysemy.Input (Input)
import           Polysemy.State       (State)


type Sampleable :: Type -> Constraint
class Sampleable a where
  sample :: BS.ByteString -> a


type IOP
  :: Type -- c
  -> Type -- t
  -> Type -- w
  -> Type -- s
  -> (Type -> Type) -- m
  -> Type -- a
  -> Type
data IOP c t w s m a where
  Setup :: IOP c t w s m s
  GetTranscript :: IOP c t w s m t
  AppendToTranscript :: t -> IOP c t w s m ()
  Reject :: IOP c t w s m ()
  SampleChallenge :: IOP c t w s m c

makeSem ''IOP

proverFiatShamir
  :: Sampleable c
  => Serialise t
  => Members '[State t] r
  => Members '[Input w] r
  => Members '[Input s] r
  => Monoid t
  => w -> Sem (IOP c t w s ': r) a -> Sem r a
proverFiatShamir = todo

verifierFiatShamir
  :: Sampleable c
  => Serialise t
  => Members '[Input [t]] r
  => Members '[Input s] r
  => Sem (IOP c t w s ': r) a -> Sem r a
verifierFiatShamir = todo


todo :: a
todo = todo


-- 
-- 
-- -- Way 1
-- 
-- verify :: Statement -> Transcript -> Sem (IOP Challenge Transcript : r) Bool
-- 
-- prove :: Statement -> Witness -> Sem (IOP Challenge Transcript : r) Bool
-- 
-- fiatShamir (prove stmt witness)
-- 
-- fiatShamir (verify stmt proof)
-- 
-- r <- fiatShamir (verify stmt (fiatShamir (prove stmt witness)))
-- r === True
-- 
-- -- Way 2
-- 
-- prove :: Statement -> Witness -> Sem r Transcript
-- 
-- verify :: Statement -> Transcript -> Sem (IOP Challenge Transcript : r) Bool
-- 
-- prove' :: Statement -> Witness -> Sem (IOP Challenge Transcript : r) Bool
-- prove' stmt witness = verify stmt =<< prove stmt witness
-- 
-- makeSem ''IOP
-- 
-- fiatShamir
--   :: Sampleable c
--   => Serialise t
--   => Members '[State t] r
--   => Monoid t
--   => Sem (IOP c t ': r) a -> Sem r a
-- fiatShamir = interpret $
--   \case
--     AppendToTranscript t -> do
--      put . (<> t) =<< get
--     SampleChallenge -> do
--      x :: q <- get
--      pure (sample (BSL.toStrict (serialise x)))
-- 
-- 
-- -- Way 3
-- 
-- newtype IOP c t m a =
--   IOP [m (RoundFunction c t m a)]
--   deriving newtype Monoid
-- 
-- 
-- data RoundFunction c t m a =
--     ProverRound (t -> m a)
--   | VerifierRound (t -> VerifierResult c)
-- 
-- 
-- data VerifierResult c =
--     AcceptsFinally
--   | Rejects
--   | Challenges c
-- 
-- 
-- -- Way 4
-- 
-- data IOP c t m a w =
--   IOP
--   { prove :: t -> w -> m t
--   , verify :: t -> m Bool
--   }
