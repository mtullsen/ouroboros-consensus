{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.GenesisTest (tests) where

import           Control.Monad.IOSim (runSimOrThrow)
import           Data.List (foldl')
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (Fingerprint (..),
                     WithFingerprint (..))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Mock.Chain (Chain)
import qualified Ouroboros.Network.Mock.Chain as Chain
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.TestBlock
import           Test.Util.Orphans.IOLike ()
import Debug.Trace (trace)
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Random (QCGen)
import Test.Ouroboros.Consensus.ChainGenerator.Tests.Adversarial hiding (tests)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Adversarial as A
import qualified Test.Ouroboros.Consensus.ChainGenerator.Honest as H
import Test.Ouroboros.Consensus.ChainGenerator.Slot (S, POL (mkActive))
import Test.Ouroboros.Consensus.ChainGenerator.Counting (getVector, lengthV, getCount)
import qualified Data.Vector.Unboxed as Vector
import qualified Test.Ouroboros.Consensus.ChainGenerator.Slot as S
import Test.Ouroboros.Consensus.ChainGenerator.Params (Asc (Asc))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Test.Ouroboros.Consensus.ChainGenerator.Tests.Sync
import Test.Ouroboros.Consensus.ChainGenerator.Tests.SyncTest (computePastLedger, computeHeaderStateHistory)
import Control.Monad.Class.MonadTime (MonadTime)
import Ouroboros.Network.Block (Tip)
import Data.Monoid (First(First, getFirst))

tests :: TestTree
tests = testGroup "Genesis tests"
    [ testProperty "blargh" prop_syncGenesis
    ]

prop_syncGenesis :: SomeTestAdversarial -> QCGen -> QC.Property
prop_syncGenesis (SomeTestAdversarial _ _ TestAdversarial {testAscH, testAscA, testRecipeA, testRecipeA'}) seed =
  runSimOrThrow $ runTest (exampleTestSetup testAscH testAscA testRecipeA testRecipeA' seed)

exampleTestSetup ::
  Asc ->
  Asc ->
  A.AdversarialRecipe base hon ->
  A.SomeCheckedAdversarialRecipe base hon ->
  QCGen ->
  TestSetup
exampleTestSetup (Asc ascH) ascA A.AdversarialRecipe {A.arHonest} (A.SomeCheckedAdversarialRecipe _ recipeA') seed =
  trace (condense goodChain) $
  trace (condense badChain) $
  TestSetup {
    secParam      = SecurityParam k
  , genesisWindow = GenesisWindow (SlotNo (ceiling (3 * fromIntegral k / ascH)))
  , goodChain
  , badChain
  }
  where
    k = 3

    goodChain = mkTestChain (firstBlock 0) slotsH

    badChain = mkTestChain (forkBlock (firstBlock 0)) slotsA

    slotsH = Vector.toList (getVector vH)

    slotsA = Vector.toList (getVector vA) <> replicate diff (mkActive S.notInverted)

    diff = getCount (lengthV vH) - getCount (lengthV vA)

    incSlot :: SlotNo -> TestBlock -> TestBlock
    incSlot n b = b { tbSlot = tbSlot b + n }

    mkTestChain :: TestBlock -> [S] -> AnchoredFragment TestBlock
    mkTestChain z bs =
      AF.fromNewestFirst AF.AnchorGenesis (NonEmpty.toList (fst (foldl' folder (z :| [], 0) bs)))
      where
        folder (h :| t, inc) s | S.test S.notInverted s = (incSlot inc (successorBlock h) :| h : t, 0)
                               | otherwise = (h :| t, inc + 1)

    H.ChainSchedule _ vH = arHonest
    H.ChainSchedule _ vA = A.uniformAdversarialChain (Just ascA) recipeA' seed

newtype GenesisWindow = GenesisWindow { getGenesisWindow :: SlotNo }
  deriving stock (Show)

data TestSetup = TestSetup {
    secParam      :: SecurityParam
  , genesisWindow :: GenesisWindow
  , goodChain     :: AnchoredFragment TestBlock
    -- has to be less dense than goodChain in the genesisWindow
  , badChain      :: AnchoredFragment TestBlock
  }
  deriving stock (Show)

intersectWith ::
  IOLike m =>
  StrictTVar m (AnchoredFragment TestBlock) ->
  AnchoredFragment TestBlock ->
  Tip TestBlock ->
  [Point TestBlock] ->
  m (Tip TestBlock, Maybe (AnchoredFragment TestBlock))
intersectWith frag fullFrag tip pts = do
  frag' <- atomically $ stateTVar frag  \ currentFrag ->
    case getFirst $ foldMap (First . AF.splitAfterPoint fullFrag) pts of
      Just (_, frag') -> (Just frag', frag')
      Nothing -> (Nothing, currentFrag)
  pure (tip, frag')

nextWith ::
  IOLike m =>
  StrictTVar m (AnchoredFragment TestBlock) ->
  Tip TestBlock ->
  m (Tip TestBlock, Maybe TestBlock)
nextWith frag tip = do
  blk <- atomically $ stateTVar frag \case
    f@(AF.Empty _) -> (Nothing, f)
    blk AF.:< frag' -> (Just blk, frag')
  pure (tip, blk)

basic ::
  IOLike m =>
  StrictTVar m (AnchoredFragment TestBlock) ->
  m (ChainMsg m)
basic frag = do
  full <- atomically (readTVar frag)
  let tip = AF.anchorToTip . AF.headAnchor $ full
  pure (ChainMsg (intersectWith frag full tip) (nextWith frag tip))

goodServer ::
  IOLike m =>
  AnchoredFragment TestBlock ->
  m (Server m)
goodServer chain = do
  frag <- uncheckedNewTVarM chain
  msg <- basic frag
  mkServer (ServeChainHonest msg) []

badServer ::
  IOLike m =>
  AnchoredFragment TestBlock ->
  m (Server m)
badServer chain = do
  frag <- uncheckedNewTVarM chain
  msg <- basic frag
  mkServer (ServeChainDelayed 0.1 msg) []

runTest ::
     forall m. IOLike m
  => MonadTime m
  => TestSetup
  -> m Property
runTest TestSetup{secParam, goodChain, badChain} = do
    g <- goodServer goodChain
    b <- badServer badChain
    let chainDbView cfg = mkChainDbView cfg g.candidate b.candidate

    res <- syncPeers chainDbView secParam [g, b]

    finalGoodCandidate <- readTVarIO g.candidate
    finalBadCandidate  <- readTVarIO b.candidate
    pure
      $ counterexample ("result: " <> show res)
      $ counterexample ("final good candidate: " <> condense finalGoodCandidate)
      $ counterexample ("final bad candidate: " <> condense finalGoodCandidate)
      $ conjoin [
          property $ res == Right ()
        , property $ not $ forksWithinK finalGoodCandidate finalBadCandidate
        ]
  where
    SecurityParam k = secParam

    forksWithinK
      :: AnchoredFragment (Header TestBlock)  -- ^ Our chain
      -> AnchoredFragment (Header TestBlock)  -- ^ Their chain
      -> Bool
    forksWithinK ourChain theirChain = case AF.intersect ourChain theirChain of
      Nothing -> False
      Just (_ourPrefix, _theirPrefix, ourSuffix, _theirSuffix) ->
        fromIntegral (AF.length ourSuffix) <= k

    mkChainDbView ::
         TopLevelConfig TestBlock
      -> StrictTVar m (AnchoredFragment (Header TestBlock))
      -> StrictTVar m (AnchoredFragment (Header TestBlock))
      -> ChainDbView m TestBlock
    mkChainDbView nodeCfg varGoodCandidate varBadCandidate = ChainDbView {
          getCurrentChain       = AF.anchorNewest k <$> getCurFrag
        , getHeaderStateHistory =
            computeHeaderStateHistory nodeCfg <$> getFullChain
        , getPastLedger         = \pt ->
            computePastLedger nodeCfg pt <$> getFullChain
        , getIsInvalidBlock     =
            pure $ WithFingerprint (pure Nothing) (Fingerprint 0)
        }
      where
        getCurFrag :: STM m (AnchoredFragment (Header TestBlock))
        getCurFrag = do
            goodCand <- readTVar varGoodCandidate
            badCand  <- readTVar varBadCandidate
            pure $
              if AF.headBlockNo goodCand > AF.headBlockNo badCand
              then goodCand
              else badCand

        getFullChain :: STM m (Chain TestBlock)
        getFullChain = do
            curFrag <- getCurFrag
            let goodOrBad =
                  if castPoint (AF.headPoint curFrag) `AF.withinFragmentBounds` goodChain
                  then goodChain
                  else badChain
                curFragTipBlockNo =
                  fromIntegral . unBlockNo . withOrigin 0 id $ AF.headBlockNo curFrag
            pure
              . Chain.fromOldestFirst
              . take curFragTipBlockNo
              . AF.toOldestFirst
              $ goodOrBad
