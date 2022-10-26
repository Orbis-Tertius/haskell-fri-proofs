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
    getMaxLowDegree,
    evalDomain,
    splitAndFold,
  )
where

import Codec.Serialise (Serialise, serialise)
import Control.Lens ((^.), _1, _2, _3)
import Control.Monad (forM_, when)
import Crypto.Number.Basic (log2)
import Data.Bits (shift, xor)
import Data.ByteString (ByteString, unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.Kind (Constraint, Type)
import Data.List.Extra (zip5, zipWith6, (!?))
import Data.Monoid (Last (Last), getLast)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Polysemy (Effect, Member, Sem, interpret, makeSem, run)
import Polysemy.Error (Error, runError, throw)
import Polysemy.Input (Input, input, runInputConst)
import Polysemy.State (State, evalState, execState, get, put, runState)
import qualified Stark.BinaryTree as BinaryTree
import Stark.Cast (intToInteger, word64ToInt)
import Stark.Fri.Types (A (A, unA), ABC, AuthPaths (AuthPaths, unAuthPaths), B (B, unB), C (C), Challenge (Challenge, unChallenge), Codeword (Codeword, unCodeword), DomainLength (DomainLength, unDomainLength), ExpansionFactor (ExpansionFactor), FriConfiguration, ListSize (ListSize), NumColinearityTests (unNumColinearityTests), Offset (Offset, unOffset), Omega (Omega, unOmega), Query (Query), RandomSeed (RandomSeed), ReducedIndex (ReducedIndex), ReducedListSize (ReducedListSize, unReducedListSize), SampleSize (SampleSize), randomSeed)
import Stark.Hash (hash)
import qualified Stark.MerkleTree as Merkle
import Stark.Prelude (uncurry5)
import Stark.Types.AuthPath (AuthPath)
import Stark.Types.CapCommitment (CapCommitment)
import Stark.Types.CapLength (CapLength)
import Stark.Types.FiatShamir (ErrorMessage (ErrorMessage), IOP, Transcript (Transcript), TranscriptPartition (TranscriptPartition), proverFiatShamir, respond, sampleChallenge, verifierFiatShamir)
import Stark.Types.Index (Index (Index, unIndex))
import Stark.Types.Scalar (Scalar, normalize)
import Stark.Types.UnivariatePolynomial (UnivariatePolynomial)
import Stark.UnivariatePolynomial (areColinear, degree, evaluate, interpolate)

type FriResponse :: Type
data FriResponse
  = Commit CapCommitment
  | Challenge' Challenge
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
  deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

type FriDSL :: (Type -> Type) -> Type -> Type
data FriDSL m a where
  GetConfig :: FriDSL m FriConfiguration
  GetInitialCommitment :: FriDSL m CapCommitment
  GetCommitment :: RoundIndex -> Challenge -> FriDSL m CapCommitment
  -- in the prover, returns the passed in challenge.
  -- in the verifier, returns the challenge the prover used.
  GetTranscriptChallenge :: RoundIndex -> Challenge -> FriDSL m Challenge
  GetLastCodeword :: FriDSL m (Last Codeword)
  GetQueries :: RoundIndex -> Challenge -> [(A Index, B Index)] -> FriDSL m ([Query], [AuthPaths])

makeSem ''FriDSL

type FriEffects :: [Effect] -> Constraint
type FriEffects r = (Member FriIOP r, Member FriDSL r, Member (Error ErrorMessage) r)

prove :: FriConfiguration -> Codeword -> Either ErrorMessage (Transcript FriResponse)
prove c w = do
  run $
    runInputConst c $
      runError @ErrorMessage $
        execState @(Transcript FriResponse) mempty $
          runState (ProverState [w]) $
            proverFiatShamir $
              runFriDSLProver $ fri

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
    GetInitialCommitment -> do
      config <- input @FriConfiguration
      lastCodewordM <- getLastCodeword'
      lastCodeword <-
        maybe
          (throw "runFriDSLProver: GetInitialCommitment: no last codeword")
          pure
          (getLast lastCodewordM)
      commitCodeword (config ^. #capLength) lastCodeword
    GetCommitment i alpha -> do
      config <- input @FriConfiguration
      ProverState codewords <- get
      lastCodewordM <- getLastCodeword'
      lastCodeword <-
        maybe
          (throw "runFriDSLProver: GetCommitment: no last codeword")
          pure
          (getLast lastCodewordM)
      when (i /= RoundIndex (length codewords - 1)) $
        throw "runFriDSLProver: GetCommitment: rounds are out of order"
      let omega = getRoundOmega config i
          offset = config ^. #offset
          codeword = splitAndFold omega offset lastCodeword alpha
      commitment <- commitCodeword (config ^. #capLength) codeword
      put (ProverState (codewords <> [codeword]))
      pure commitment
    GetLastCodeword -> getLastCodeword'
    GetTranscriptChallenge _ alpha ->
      pure alpha
    GetQueries (RoundIndex i) alpha indices -> do
      config <- input @FriConfiguration
      ProverState codewords <- get
      currentCodeword <-
        maybe
          (throw "runFriDSLProver: GetQueries: missing current codeword")
          (pure . unCodeword)
          (codewords !? i)
      let aIndices = (^. _1 . #unA . #unIndex) <$> indices
          bIndices = (^. _2 . #unB . #unIndex) <$> indices
          offset = config ^. #offset
          roundOmega' = getRoundOmega config (RoundIndex i)
          roundOmega = trace ("prover round omega: " <> show (i, roundOmega')) roundOmega'
      ays <-
        maybe (throw "runFriDSLProver: GetQueries: failed a index lookups") pure $
          sequence ((currentCodeword !?) . word64ToInt <$> aIndices)
      bys <-
        maybe (throw "runFriDSLProver: GetQueries: failed a index lookups") pure $
          sequence ((currentCodeword !?) . word64ToInt <$> bIndices)
      queries <-
        maybe (throw "runFriDSLProver: GetQueries: missing leaf") pure
          . sequence
          $ zipWith6
            (\ai a bi b ci c -> Query <$> ((,,) <$> (A . (ai,) <$> a) <*> (B . (bi,) <$> b) <*> (C . (ci,) <$> c)))
            (Index <$> aIndices)
            (pure . normalize <$> ays)
            (Index <$> bIndices)
            (pure . normalize <$> bys)
            (repeat (unChallenge alpha))
            ( pure
                <$> zipWith3
                  (linearCombination roundOmega offset alpha)
                  (Index <$> aIndices)
                  ays
                  bys
            )
      let capLength = config ^. #capLength
      openingProofs <-
        sequence $
          ( \(a, b) ->
              AuthPaths
                <$> ( (,) <$> (A <$> openCodeword capLength (Codeword currentCodeword) (Index a))
                        <*> (B <$> openCodeword capLength (Codeword currentCodeword) (Index b))
                    )
          )
            <$> zip aIndices bIndices
      forM_ queries $ \q -> do
        let ai = q ^. #unQuery . _1 . #unA . _1
            bi = q ^. #unQuery . _2 . #unB . _1
            ax = normalize $ (roundOmega ^ ai) ^. #unOmega
            bx = normalize $ (roundOmega ^ bi) ^. #unOmega
            cx = q ^. #unQuery . _3 . #unC . _1
            ay = q ^. #unQuery . _1 . #unA . _2
            by = q ^. #unQuery . _2 . #unB . _2
            cy = q ^. #unQuery . _3 . #unC . _2
        colinearityCheck
          "prover"
          (RoundIndex i)
          (A (ax, ay), B (bx, by), C (cx, cy))
      pure (queries, openingProofs)
  where
    getLastCodeword' ::
      Member (State ProverState) r =>
      Sem r (Last Codeword)
    getLastCodeword' = do
      ProverState codewords <- get @ProverState
      pure $ mconcat $ Last . Just <$> codewords

runFriDSLVerifier ::
  Member (Input FriConfiguration) r =>
  Member (Input (Transcript FriResponse)) r =>
  Member (Error ErrorMessage) r =>
  Sem (FriDSL ': r) a ->
  Sem r a
runFriDSLVerifier =
  interpret $ \case
    GetConfig -> input @FriConfiguration
    GetInitialCommitment -> do
      Transcript t <- input @(Transcript FriResponse)
      reply <-
        maybe (throw "runFriDSLVerifier: GetInitialCommitment: no commitment") pure $
          (filter isCommitment t) !? 0
      case reply of
        Commit c -> pure c
        _ -> throw "runFriDSLVerifier: GetInitialCommitment: impossible happened"
    GetTranscriptChallenge (RoundIndex i) _ -> do
      Transcript t <- input @(Transcript FriResponse)
      reply <-
        maybe (throw "runFriDSLVerifier: GetTranscriptChallenge: no challenge") pure $
          (filter isChallenge t) !? i
      case reply of
        Challenge' c -> pure c
        _ -> throw "runFriDSLVerifier: GetTranscriptChallenge: impossible happened"
    GetCommitment (RoundIndex i) _alpha -> do
      Transcript t <- input @(Transcript FriResponse)
      reply <-
        maybe (throw "runFriDSLVerifier: GetCommitment: no commitment") pure $
          (filter isCommitment t) !? (i + 1)
      case reply of
        Commit c -> pure c
        _ -> throw "runFriDSLVerifier: GetCommitment: impossible happened"
    GetLastCodeword -> do
      Transcript t <- input @(Transcript FriResponse)
      case filter isLastCodeword t of
        [LastCodeword' l] -> pure l
        [] -> throw "runFriDSLVerifier: GetLastCodeword: no last codeword"
        _ -> throw "runFriDSLVerifier: GetLastCodeword: more than one last codeword"
    GetQueries (RoundIndex i) _alpha _indices -> do
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

    isChallenge (Challenge' _) = True
    isChallenge _ = False

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
  commitment0 <- getInitialCommitment
  respond (Commit commitment0)
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
  respond (LastCodeword' (Last (Just lastCodeword)))
  when (lastRoot /= expectedLastRoot) $
    throw "commitPhase: last codeword commitment check failed"
  let lastDomainLength = roundDomainLength config (RoundIndex (n - 1))
      lastOmega = getLastOmega config
      lastDomain = evalDomain (config ^. #offset) lastOmega lastDomainLength
      poly =
        interpolate $ -- TODO: use FFT
          zip lastDomain (unCodeword lastCodeword)
      maxDegree = getMaxLowDegree (config ^. #domainLength) (config ^. #expansionFactor)
  when (degree poly > maxDegree) $
    throw "commitPhase: last codeword is not low degree"
  pure (commitment0 : commitments, alphas)

commitRound ::
  FriEffects r =>
  RoundIndex ->
  Sem r (CapCommitment, Challenge)
commitRound i = do
  alpha <-
    Challenge . normalize . unChallenge
      <$> sampleChallenge
  respond (Challenge' alpha)
  alpha' <- getTranscriptChallenge i alpha
  when (alpha /= alpha') . throw . ErrorMessage $
    "Challenges in round " <> show i <> " do not match: "
      <> show (alpha, alpha')
  c <- getCommitment i alpha
  respond (Commit c)
  pure (c, alpha)

queryPhase ::
  FriEffects r =>
  [CapCommitment] ->
  [Challenge] ->
  [(Index, ReducedIndex)] ->
  Sem r ()
queryPhase commitments challenges indices = do
  go (indices, 0, commitments, challenges)
  where
    go :: FriEffects r => ([(Index, ReducedIndex)], RoundIndex, [CapCommitment], [Challenge]) -> Sem r ()
    go x@(_, RoundIndex i, _, _) = do
      config <- getConfig
      if i < numRounds config
        then go =<< queryRound x
        else pure ()

queryRound ::
  FriEffects r =>
  ([(Index, ReducedIndex)], RoundIndex, [CapCommitment], [Challenge]) ->
  Sem r ([(Index, ReducedIndex)], RoundIndex, [CapCommitment], [Challenge])
queryRound (indices, i, (root : nextRoot : remainingRoots), (alpha : alphas)) = do
  config <- getConfig
  let omega' = getRoundOmega config i
      omega = trace ("query round omega: " <> show (i, omega')) omega'
      offset = config ^. #offset
      nextIndices =
        (fst <$> indices)
          <&> ( `mod`
                  Index
                    ( unDomainLength
                        (roundDomainLength config (i + 1))
                    )
              )
      aIndices = A <$> nextIndices
      bIndices =
        B
          . ( +
                Index
                  ( unDomainLength (roundDomainLength config (i + 1))
                  )
            )
          <$> nextIndices
  (qs, ps) <- getQueries i alpha ((,) <$> aIndices <*> bIndices)
  respond (QueryRound (qs, ps))
  let as = (^. #unQuery . _1 . #unA) <$> qs
      bs = (^. #unQuery . _2 . #unB) <$> qs
      cs = (^. #unQuery . _3 . #unC) <$> qs
      ays = (^. _2) <$> as
      bys = (^. _2) <$> bs
      f :: Integral x => x -> Scalar
      f = (* unOffset offset) . (unOmega omega ^)
      points =
        zip3
          (A <$> (zip (f . unA <$> aIndices) ays))
          (B <$> (zip (f . unB <$> bIndices) bys))
          (C <$> cs)
      allPaths = unAuthPaths <$> ps
      aPaths, bPaths :: [AuthPath]
      aPaths = (^. _1 . #unA) <$> allPaths
      bPaths = (^. _2 . #unB) <$> allPaths
      authPathCheckInputs =
        mconcat
          [ zip5 (repeat "A") (repeat root) (unA <$> aIndices) aPaths as,
            zip5 (repeat "B") (repeat root) (unB <$> bIndices) bPaths bs
          ]
  forM_ authPathCheckInputs $
    uncurry5 (authPathCheck (config ^. #capLength) i)
  forM_ points (colinearityCheck "IOP" i)
  pure
    ( zip nextIndices (snd <$> indices),
      i + 1,
      nextRoot : remainingRoots,
      alphas
    )
queryRound (_, _, _ : [], _) = throw "queryRound: not enough roots (1)"
queryRound (_, _, [], _) = throw "queryRound: not enough roots (0)"
queryRound (_, _, _, []) = throw "queryRound: not enough alphas"

colinearityCheck :: Member (Error ErrorMessage) r => String -> RoundIndex -> ABC (Scalar, Scalar) -> Sem r ()
colinearityCheck s i (A a, B b, C c) =
  when (not (areColinear [a, b, c]))
    . throw
    . ErrorMessage
    $ s <> " colinearity check failed: "
      <> show (i, a, b, c)

authPathCheck :: FriEffects r => CapLength -> RoundIndex -> String -> CapCommitment -> Index -> AuthPath -> (Index, Scalar) -> Sem r ()
authPathCheck capLength i abc commitment j authPath q = do
  when
    (j /= (q ^. _1))
    (throw . ErrorMessage $ "auth path check: wrong indices: " <> show (abc, i, j, q ^. _1, commitment, authPath))
  when
    (not (Merkle.verify capLength commitment j authPath (q ^. _2)))
    (throw . ErrorMessage $ "auth path check failed: " <> show (abc, i, j, q, commitment, authPath))

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

numRounds' :: DomainLength -> ExpansionFactor -> Int
numRounds' d e =
  log2 (intToInteger (getMaxLowDegree d e))

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

getRoundOmega :: FriConfiguration -> RoundIndex -> Omega
getRoundOmega config i =
  (config ^. #omega) ^ (two ^ i)
  where
    two :: Integer
    two = 2

getLastOmega :: FriConfiguration -> Omega
getLastOmega config =
  getRoundOmega config (RoundIndex (numRounds config - 1))

evalDomain :: Offset -> Omega -> DomainLength -> [Scalar]
evalDomain (Offset o) (Omega m) (DomainLength d) =
  [o * (m ^ i) | i <- [0 .. d - 1]]

getMaxLowDegree :: DomainLength -> ExpansionFactor -> Int
getMaxLowDegree (DomainLength d) (ExpansionFactor e) =
  word64ToInt d `div` e

splitAndFold :: Omega -> Offset -> Codeword -> Challenge -> Codeword
splitAndFold omega offset (Codeword codeword) alpha =
  let (l, r) = splitAt (length codeword `quot` 2) codeword
   in Codeword $
        [ linearCombination omega offset alpha i xi xj
          | (i, xi, xj) <- zip3 [0 ..] l r
        ]

linearCombination :: Omega -> Offset -> Challenge -> Index -> Scalar -> Scalar -> Scalar
linearCombination (Omega omega) (Offset offset) (Challenge x) i ay by =
  let ax = offset * (omega ^ i)
      bx = negate ax
   in normalize $ (ay * (bx - x) + by * (x - ax)) / (bx - ax)

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
