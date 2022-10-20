{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Stark.Fri
  ( FriResponse (Commit, LastCodeword', QueryRound),
    FriIOP,
    ProverState (ProverState),
    FriDSL (GetCommitment, GetLastCodeword, GetQueries),
    runFriDSLProver,
    runFriDSLVerifier,
    getCommitment,
    getQueries,
    getLastCodeword,
    getCodeword,
    commitCodeword,
    RoundIndex (RoundIndex, unRoundIndex),
    fri,
    commitPhase,
    prove,
    verify,
  )
where

import Codec.Serialise (Serialise, serialise)
import Control.Lens ((^.), _1, _2, _3)
import Control.Monad (void, when)
import "monad-extras" Control.Monad.Extra (iterateM)
import Data.Bits (shift, xor)
import Data.ByteString (ByteString, unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.Kind (Constraint, Type)
import Data.List.Extra (zip4, (!?))
import Data.Monoid (Last (Last), getLast)
import GHC.Generics (Generic)
import Polysemy (Effect, Member, Sem, interpret, makeSem, run)
import Polysemy.Error (Error, runError, throw)
import Polysemy.Input (Input, input, runInputConst)
import Polysemy.State (State, evalState, execState, get, put, runState)
import qualified Stark.BinaryTree as BinaryTree
import Stark.Fri.Types (A (A, unA), ABC, AuthPaths (AuthPaths, unAuthPaths), B (B, unB), C (C, unC), Challenge (Challenge, unChallenge), Codeword (Codeword, unCodeword), DomainLength (DomainLength, unDomainLength), ExpansionFactor (ExpansionFactor), FriConfiguration, ListSize (ListSize), NumColinearityTests (NumColinearityTests, unNumColinearityTests), Offset (Offset, unOffset), Omega (Omega, unOmega), Query (Query), RandomSeed (RandomSeed), ReducedIndex (ReducedIndex), ReducedListSize (ReducedListSize, unReducedListSize), SampleSize (SampleSize), randomSeed)
import Stark.Hash (hash)
import qualified Stark.MerkleTree as Merkle
import Stark.Prelude (uncurry4)
import Stark.Types.AuthPath (AuthPath)
import Stark.Types.CapCommitment (CapCommitment)
import Stark.Types.CapLength (CapLength (CapLength))
import Stark.Types.FiatShamir (ErrorMessage, IOP, Transcript (Transcript), TranscriptPartition (TranscriptPartition), proverFiatShamir, sampleChallenge, verifierFiatShamir)
import Stark.Types.Index (Index (Index, unIndex))
import Stark.Types.Scalar (Scalar)
import Stark.Types.UnivariatePolynomial (UnivariatePolynomial)
import Stark.UnivariatePolynomial (areColinear, degree, evaluate, interpolate)

type FriResponse :: Type
data FriResponse
  = Commit CapCommitment
  | LastCodeword' (Last Codeword)
  | QueryRound ([Query], [AuthPaths])
  deriving stock (Eq, Show, Generic)

instance Serialise FriResponse

type FriIOP :: (Type -> Type) -> Type -> Type
type FriIOP = IOP Challenge FriResponse

type ProverState :: Type
data ProverState
  = ProverState [Codeword]

type RoundIndex :: Type
newtype RoundIndex = RoundIndex {unRoundIndex :: Int}
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral)

type FriDSL :: (Type -> Type) -> Type -> Type
data FriDSL m a where
  GetConfig :: FriDSL m FriConfiguration
  GetCommitment :: RoundIndex -> Challenge -> FriDSL m CapCommitment
  GetLastCodeword :: FriDSL m (Last Codeword)
  GetQueries :: RoundIndex -> [ABC Index] -> FriDSL m ([Query], [AuthPaths])

makeSem ''FriDSL

type FriEffects :: [Effect] -> Constraint
type FriEffects r = (Member FriIOP r, Member FriDSL r, Member (Error ErrorMessage) r)

prove :: FriConfiguration -> Codeword -> Either ErrorMessage (Transcript FriResponse)
prove c w = run $ runInputConst c $ runError @ErrorMessage $ execState @(Transcript FriResponse) mempty $ runState (ProverState [w]) $ proverFiatShamir $ runFriDSLProver $ fri

verify :: FriConfiguration -> Transcript FriResponse -> Either ErrorMessage ()
verify c t = run $ evalState (TranscriptPartition (mempty, t)) $ runInputConst t $ runInputConst c $ runError @ErrorMessage $ verifierFiatShamir $ runFriDSLVerifier $ fri

-- It assumes that the state initially contains a list of length 1,
-- with the largest codeword as its element.
runFriDSLProver ::
  Member (Input FriConfiguration) r =>
  Member (State ProverState) r =>
  Member (Error ErrorMessage) r =>
  Sem (FriDSL ': r) a ->
  Sem r a
runFriDSLProver =
  interpret $ \case
    GetConfig -> input @FriConfiguration
    GetCommitment i alpha -> do
      config <- input @FriConfiguration
      ProverState codewords <- get
      lastCodewordM <- getLastCodeword'
      lastCodeword <-
        maybe
          (throw "runFriDSLProver: GetCommitment: no last codeword")
          pure
          (getLast lastCodewordM)
      when (i /= RoundIndex (length codewords + 1)) $
        throw "runFriDSLProver: GetCommitment: rounds are out of order"
      let omega = (config ^. #omega) ^ (two ^ i)
          offset = (config ^. #offset) ^ (two ^ i)
          codeword = splitAndFold omega offset (lastCodeword) alpha
      commitment <- commitCodeword (config ^. #capLength) codeword
      put (ProverState (codewords <> [codeword]))
      pure commitment
    GetLastCodeword -> getLastCodeword'
    GetQueries (RoundIndex i) indices -> do
      config <- input @FriConfiguration
      ProverState codewords <- get
      currentCodeword <-
        maybe
          (throw "runFriDSLProver: GetQueries: missing current codeword")
          (pure . unCodeword)
          (codewords !? i)
      nextCodeword <-
        maybe
          (throw "runFriDSLProver: GetQueries: missing next codeword")
          (pure . unCodeword)
          (codewords !? (i + 1))
      let aIndices = (^. _1 . #unA . #unIndex) <$> indices
          bIndices = (^. _2 . #unB . #unIndex) <$> indices
          cIndices = (^. _3 . #unC . #unIndex) <$> indices
      queries <-
        maybe (throw "runFriDSLProver: GetQueries: missing leaf") pure
          . sequence
          $ zipWith3
            (\a b c -> Query <$> ((,,) <$> (A <$> a) <*> (B <$> b) <*> (C <$> c)))
            ((currentCodeword !?) <$> aIndices)
            ((currentCodeword !?) <$> bIndices)
            ((nextCodeword !?) <$> cIndices)
      let capLength = config ^. #capLength
      openingProofs <-
        sequence $
          ( \(a, b, c) ->
              AuthPaths
                <$> ( (,,) <$> (A <$> openCodeword capLength (Codeword currentCodeword) (Index a))
                        <*> (B <$> openCodeword capLength (Codeword currentCodeword) (Index b))
                        <*> (C <$> openCodeword capLength (Codeword nextCodeword) (Index c))
                    )
          )
            <$> zip3 aIndices bIndices cIndices
      pure (queries, openingProofs)
  where
    getLastCodeword' ::
      Member (State ProverState) r =>
      Sem r (Last Codeword)
    getLastCodeword' = do
      ProverState codewords <- get @ProverState
      pure $ mconcat $ Last . Just <$> codewords

    two :: Integer
    two = 2

runFriDSLVerifier ::
  Member (Input FriConfiguration) r =>
  Member (Input (Transcript FriResponse)) r =>
  Member (Error ErrorMessage) r =>
  Sem (FriDSL ': r) a ->
  Sem r a
runFriDSLVerifier =
  interpret $ \case
    GetConfig -> input @FriConfiguration
    GetCommitment (RoundIndex i) _alpha -> do
      Transcript t <- input @(Transcript FriResponse)
      reply <-
        maybe (throw "runFriDSLVerifier: GetCommitment: no commitment") pure $
          (filter isCommitment t) !? i
      case reply of
        Commit c -> pure c
        _ -> throw "runFriDSLVerifier: GetCommitment: impossible happened"
    GetLastCodeword -> do
      Transcript t <- input @(Transcript FriResponse)
      case filter isLastCodeword t of
        [LastCodeword' l] -> pure l
        [] -> throw "runFriDSLVerifier: GetLastCodeword: no last codeword"
        _ -> throw "runFriDSLVerifier: GetLastCodeword: more than one last codeword"
    GetQueries (RoundIndex i) _indices -> do
      Transcript t <- input @(Transcript FriResponse)
      reply <-
        maybe (throw "runFriDSLVerifier: GetQueries: no queries") pure $
          (filter isQueryRound t) !? i
      case reply of
        QueryRound r -> pure r
        _ -> throw "runFriDSLVerifier: GetQueries: impossible happened"
  where
    isCommitment (Commit _) = True
    isCommitment _ = False

    isLastCodeword (LastCodeword' _) = True
    isLastCodeword _ = False

    isQueryRound (QueryRound _) = True
    isQueryRound _ = False

fri ::
  Member (Error ErrorMessage) r =>
  Member FriIOP r =>
  Member FriDSL r =>
  Sem r ()
fri = do
  config <- getConfig
  (commitments, alphas) <- commitPhase
  lastCommitPhaseAlpha <-
    maybe (throw "no commitPhase alphas") pure $
      alphas !? (length alphas - 1)
  let n = numRounds config
      indices =
        sampleIndices
          (randomSeed lastCommitPhaseAlpha)
          (ListSize (unDomainLength (roundDomainLength config 1)))
          ( ReducedListSize
              ( unDomainLength
                  (roundDomainLength config (RoundIndex n))
              )
          )
          (SampleSize (unNumColinearityTests (config ^. #numColinearityTests)))
  queryPhase commitments alphas indices

sampleIndices ::
  RandomSeed ->
  ListSize ->
  ReducedListSize ->
  SampleSize ->
  [(Index, ReducedIndex)]
sampleIndices (RandomSeed seed) ls rls (SampleSize sampleSize) =
  [ (index, reducedIndex)
    | counter <- [1 .. sampleSize],
      let index = sampleIndex (hash (seed <> toStrict (serialise counter))) ls,
      let reducedIndex = ReducedIndex $ unIndex index `mod` unReducedListSize rls
  ]

commitPhase ::
  FriEffects r =>
  Sem r ([CapCommitment], [Challenge])
commitPhase = do
  config <- getConfig
  let n = numRounds config
  (commitments, alphas) <-
    unzip
      <$> sequence (commitRound <$> [0 .. RoundIndex (n - 1)])
  lastRoot <-
    maybe (throw "commitPhase: no last root") pure $
      commitments !? (length commitments - 1)
  lastCodeword <-
    maybe (throw "commitPhase: no last codeword") pure . getLast
      =<< getLastCodeword
  expectedLastRoot <- commitCodeword (config ^. #capLength) lastCodeword
  when (lastRoot /= expectedLastRoot) $
    throw "commitPhase: last codeword commitment check failed"
  let lastDomainLength = roundDomainLength config (RoundIndex (n - 1))
      lastOmega = getLastOmega config
      lastOffset = getLastOffset config
      lastDomain = evalDomain lastOffset lastOmega lastDomainLength
      poly =
        interpolate $ -- TODO: use FFT
          zip lastDomain (unCodeword lastCodeword)
      maxDegree = getMaxDegree (config ^. #domainLength)
  when (degree poly > maxDegree) $
    throw "commitPhase: last codeword is not low degree"
  pure (commitments, alphas)

commitRound ::
  Member FriIOP r =>
  Member FriDSL r =>
  RoundIndex ->
  Sem r (CapCommitment, Challenge)
commitRound i = do
  alpha <- sampleChallenge
  (,alpha) <$> getCommitment i alpha

queryPhase ::
  FriEffects r =>
  [CapCommitment] ->
  [Challenge] ->
  [(Index, ReducedIndex)] ->
  Sem r ()
queryPhase commitments challenges indices = do
  config <- getConfig
  void
    ( take (numRounds config)
        <$> iterateM queryRound (indices, 0, commitments, challenges)
    )

queryRound ::
  FriEffects r =>
  ([(Index, ReducedIndex)], RoundIndex, [CapCommitment], [Challenge]) ->
  Sem r ([(Index, ReducedIndex)], RoundIndex, [CapCommitment], [Challenge])
queryRound (indices, i, (root : nextRoot : remainingRoots), (alpha : alphas)) = do
  config <- getConfig
  let capLength = config ^. #capLength
      omega = (config ^. #omega) ^ ((2 :: Integer) ^ i)
      offset = (config ^. #offset) ^ ((2 :: Integer) ^ i)
      nextIndices =
        (fst <$> indices)
          <&> ( `mod`
                  Index
                    ( unDomainLength
                        (roundDomainLength config (i + 1))
                    )
              )
      aIndices = A . fst <$> indices
      bIndices =
        B
          . ( +
                Index
                  ( unDomainLength (roundDomainLength config i)
                      `quot` 2
                  )
            )
          . fst
          <$> indices
      cIndices = C . fst <$> indices
  (qs, ps) <- getQueries i ((,,) <$> aIndices <*> bIndices <*> cIndices)
  let ays = (^. #unQuery . _1) <$> qs
      bys = (^. #unQuery . _2) <$> qs
      cys = (^. #unQuery . _3) <$> qs
      f :: Integral x => x -> Scalar
      f = (* unOffset offset) . (unOmega omega ^)
      colinearityChecks =
        all areColinear $
          (\(a, b, c) -> [a, b, c])
            <$> zip3
              (zip (f . unA <$> aIndices) (unA <$> ays))
              (zip (f . unB <$> bIndices) (unB <$> bys))
              (zip (repeat (unChallenge alpha)) (unC <$> cys))
      allPaths = unAuthPaths <$> ps
      aPaths = (^. _1 . #unA) <$> allPaths
      bPaths = (^. _2 . #unB) <$> allPaths
      cPaths = (^. _3 . #unC) <$> allPaths
      aAuthPathChecks =
        all (uncurry4 (Merkle.verify capLength)) $
          zip4 (repeat root) (unA <$> aIndices) aPaths (unA <$> ays)
      bAuthPathChecks =
        all (uncurry4 (Merkle.verify capLength)) $
          zip4 (repeat root) (unB <$> bIndices) bPaths (unB <$> bys)
      cAuthPathChecks =
        all (uncurry4 (Merkle.verify capLength)) $
          zip4 (repeat nextRoot) (unC <$> cIndices) cPaths (unC <$> cys)
      authPathChecks = aAuthPathChecks && bAuthPathChecks && cAuthPathChecks
  when (not colinearityChecks) (throw "colinearity check failed")
  when (not authPathChecks) (throw "auth path check failed")
  pure
    ( zip nextIndices (snd <$> indices),
      i + 1,
      nextRoot : remainingRoots,
      alphas
    )
queryRound (_, _, _ : [], _) = throw "queryRound: not enough roots (1)"
queryRound (_, _, [], _) = throw "queryRound: not enough roots (0)"
queryRound (_, _, _, []) = throw "queryRound: not enough alphas"

roundDomainLength ::
  FriConfiguration ->
  RoundIndex ->
  DomainLength
roundDomainLength config i =
  (config ^. #domainLength) `quot` (2 ^ i)

numRounds :: FriConfiguration -> Int
numRounds config =
  numRounds'
    (config ^. #domainLength)
    (config ^. #expansionFactor)
    (config ^. #numColinearityTests)
    (config ^. #capLength)

numRounds' :: DomainLength -> ExpansionFactor -> NumColinearityTests -> CapLength -> Int
numRounds' (DomainLength d) (ExpansionFactor e) (NumColinearityTests n) (CapLength n') =
  if fromIntegral d > e && d > n' && 4 * n < d
    then
      1
        + numRounds'
          (DomainLength (d `div` 2))
          (ExpansionFactor e)
          (NumColinearityTests n)
          (CapLength n')
    else 0

sampleIndex :: ByteString -> ListSize -> Index
sampleIndex bs (ListSize len) =
  foldl (\acc b -> (acc `shift` 8) `xor` fromIntegral b) 0 (unpack bs)
    `mod` Index len

commitCodeword ::
  Member (Error ErrorMessage) r =>
  CapLength ->
  Codeword ->
  Sem r CapCommitment
commitCodeword capLength =
  fmap (Merkle.commit capLength)
    . maybe (throw "codeword is not a binary tree") pure
    . BinaryTree.fromList
    . unCodeword

getLastOmega :: FriConfiguration -> Omega
getLastOmega config =
  let nr = numRounds config
   in (config ^. #omega) ^ (2 * (nr - 1))

getLastOffset :: FriConfiguration -> Offset
getLastOffset config =
  let nr = numRounds config
   in (config ^. #offset) ^ (2 * (nr - 1))

evalDomain :: Offset -> Omega -> DomainLength -> [Scalar]
evalDomain (Offset o) (Omega m) (DomainLength d) =
  [o * (m ^ i) | i <- [0 .. d - 1]]

getMaxDegree :: DomainLength -> Int
getMaxDegree (DomainLength d) = floor (logBase 2 (fromIntegral d) :: Double)

splitAndFold :: Omega -> Offset -> Codeword -> Challenge -> Codeword
splitAndFold (Omega omega) (Offset offset) (Codeword codeword) (Challenge alpha) =
  let (l, r) = splitAt (length codeword `quot` 2) codeword
   in Codeword $
        [ recip 2
            * ( (1 + alpha / (offset * (omega ^ i))) * xi
                  + (1 - alpha / (offset * (omega ^ i))) * xj
              )
          | (i, xi, xj) <- zip3 [(0 :: Integer) ..] l r
        ]

openCodeword ::
  Member (Error ErrorMessage) r =>
  CapLength ->
  Codeword ->
  Index ->
  Sem r AuthPath
openCodeword capLength (Codeword xs) i = do
  xs' <- maybe (throw "codeword is not a binary tree") pure (BinaryTree.fromList xs)
  pure $ Merkle.open capLength i xs'

getCodeword :: FriConfiguration -> UnivariatePolynomial Scalar -> Codeword
getCodeword config poly =
  Codeword $
    evaluate poly
      <$> evalDomain (config ^. #offset) (config ^. #omega) (config ^. #domainLength)
