{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.SyncTest (
  tests,
  computeHeaderStateHistory,
  computePastLedger,
) where

-- Test that the current code can't sync safely (i.e. is vulnerable to naive
-- long-range attacks) when an adversary is sending blocks quickly.
--
--  * Two ChainSync clients
--  * Mocked chain selection logic (longest chain rule)

import           Cardano.Crypto.DSIGN (SignKeyDSIGN (..), VerKeyDSIGN (..))
import           Cardano.Slotting.Time (SlotLength, slotLengthFromSec)
import           Control.Monad
import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Tracer
import           Data.Functor
import           Data.List (foldl')
import qualified Data.Map.Strict as Map
import           Data.Monoid (First (..))
import           Network.TypedProtocol.Channel (createConnectedChannels)
import           Network.TypedProtocol.Driver.Simple
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (Fingerprint (..),
                     WithFingerprint (..))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Tip (..))
import           Ouroboros.Network.ControlMessage (ControlMessage (..))
import           Ouroboros.Network.Mock.Chain (Chain)
import qualified Ouroboros.Network.Mock.Chain as Chain
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
                     (chainSyncClientPeerPipelined)
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSyncId)
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (pipelineDecisionLowHighMark)
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.TestBlock
import           Test.Util.Orphans.IOLike ()
import Ouroboros.Consensus.HeaderStateHistory (HeaderStateHistory)
import qualified Ouroboros.Consensus.HeaderStateHistory as HeaderStateHistory
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState, ExtLedgerCfg (ExtLedgerCfg))
import Ouroboros.Consensus.Ledger.Basics (getTip)
import Ouroboros.Consensus.Ledger.Abstract (tickThenReapply)
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

-- | Simulates 'ChainDB.getPastLedger'.
computePastLedger ::
     TopLevelConfig TestBlock
  -> Point TestBlock
  -> Chain TestBlock
  -> Maybe (ExtLedgerState TestBlock)
computePastLedger cfg pt chain
    | pt `elem` validPoints
    = Just $ go testInitExtLedger (Chain.toOldestFirst chain)
    | otherwise
    = Nothing
  where
    SecurityParam k = configSecurityParam cfg

    curFrag :: AnchoredFragment TestBlock
    curFrag =
          AF.anchorNewest k
        . Chain.toAnchoredFragment
        $ chain

    validPoints :: [Point TestBlock]
    validPoints =
        AF.anchorPoint curFrag : map blockPoint (AF.toOldestFirst curFrag)

    -- | Apply blocks to the ledger state until we have applied the block
    -- matching @pt@, after which we return the resulting ledger.
    --
    -- PRECONDITION: @pt@ is in the list of blocks or genesis.
    go :: ExtLedgerState TestBlock -> [TestBlock] -> ExtLedgerState TestBlock
    go !st blks
        | castPoint (getTip st) == pt
        = st
        | blk:blks' <- blks
        = go (tickThenReapply (ExtLedgerCfg cfg) blk st) blks'
        | otherwise
        = error "point not in the list of blocks"

-- | Simulates 'ChainDB.getHeaderStateHistory'.
computeHeaderStateHistory ::
     TopLevelConfig TestBlock
  -> Chain TestBlock
  -> HeaderStateHistory TestBlock
computeHeaderStateHistory cfg =
      HeaderStateHistory.trim (fromIntegral k)
    . HeaderStateHistory.fromChain cfg testInitExtLedger
  where
    SecurityParam k = configSecurityParam cfg

tests :: TestTree
tests = testGroup "SyncingTest"
    [ testProperty "basic" prop_syncGenesis
    ]

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

prop_syncGenesis :: SomeTestAdversarial -> QCGen -> QC.Property
prop_syncGenesis (SomeTestAdversarial _ _ TestAdversarial {testAscH, testAscA, testRecipeA, testRecipeA'}) seed =
  runSimOrThrow $ runTest ( (exampleTestSetup testAscH testAscA testRecipeA testRecipeA' seed))

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

runTest ::
     forall m. IOLike m
  => TestSetup
  -> m Property
runTest TestSetup{secParam, goodChain, badChain} = do
    varGoodCandidate <- uncheckedNewTVarM $ AF.Empty AF.AnchorGenesis
    varBadCandidate  <- uncheckedNewTVarM $ AF.Empty AF.AnchorGenesis
    let chainDbView = mkChainDbView varGoodCandidate varBadCandidate
        runChainSync client server = do
          runConnectedPeersPipelined
            createConnectedChannels
            nullTracer
            codecChainSyncId
            (chainSyncClientPeerPipelined client)
            (chainSyncServerPeer server)

        runClients = race (threadDelay 10) $
            runChainSync
              (theChainSyncClient chainDbView varGoodCandidate)
              (boringChainSyncServer (threadDelay 0.1) goodChain)
          `concurrently`
            runChainSync
              (theChainSyncClient chainDbView varBadCandidate)
              (boringChainSyncServer (threadDelay 0.05) badChain)

    res :: Either ChainSyncClientException () <- try (void runClients)

    finalGoodCandidate <- readTVarIO varGoodCandidate
    finalBadCandidate  <- readTVarIO varBadCandidate
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
         StrictTVar m (AnchoredFragment (Header TestBlock))
      -> StrictTVar m (AnchoredFragment (Header TestBlock))
      -> ChainDbView m TestBlock
    mkChainDbView varGoodCandidate varBadCandidate = ChainDbView {
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

    theChainSyncClient chainDbView varCandidate =
      chainSyncClient
        (pipelineDecisionLowHighMark 10 20)
        nullTracer
        nodeCfg
        chainDbView
        maxBound
        (return Continue)
        nullTracer
        varCandidate

    boringChainSyncServer ::
         m ()
      -> AnchoredFragment TestBlock
      -> ChainSyncServer (Header TestBlock) (Point TestBlock) (Tip TestBlock) m ()
    boringChainSyncServer delay fullFrag = go fullFrag
      where
        go :: AnchoredFragment TestBlock -> ChainSyncServer (Header TestBlock) (Point TestBlock) (Tip TestBlock) m ()
        go frag = ChainSyncServer $ pure ServerStIdle {
              recvMsgRequestNext = case frag of
                  AF.Empty _ -> pure $ Right $ forever $ threadDelay 1000
                  blk AF.:< frag' -> delay $> do
                    Left $ SendMsgRollForward (getHeader blk) tip (go frag')
            , recvMsgFindIntersect = \pts ->
                pure $ case getFirst $ foldMap (First . AF.splitAfterPoint fullFrag) pts of
                  Just (_, frag') -> SendMsgIntersectFound (AF.anchorPoint frag') tip (go frag')
                  Nothing         -> SendMsgIntersectNotFound tip (go frag)
            , recvMsgDoneClient = pure ()
            }

        tip = AF.anchorToTip . AF.headAnchor $ fullFrag

    numCoreNodes = NumCoreNodes 2

    slotLength :: SlotLength
    slotLength = slotLengthFromSec 20

    eraParams :: HardFork.EraParams
    eraParams = HardFork.defaultEraParams secParam slotLength

    nodeCfg :: TopLevelConfig TestBlock
    nodeCfg = TopLevelConfig {
        topLevelConfigProtocol = BftConfig {
            bftParams  = BftParams {
                             bftSecurityParam = secParam
                           , bftNumNodes      = numCoreNodes
                           }
          , bftSignKey = SignKeyMockDSIGN 0
          , bftVerKeys = Map.fromList [
                             (CoreId (CoreNodeId 0), VerKeyMockDSIGN 0)
                           , (CoreId (CoreNodeId 1), VerKeyMockDSIGN 1)
                           ]
          }
      , topLevelConfigLedger  = eraParams
      , topLevelConfigBlock   = TestBlockConfig numCoreNodes
      , topLevelConfigCodec   = TestBlockCodecConfig
      , topLevelConfigStorage = TestBlockStorageConfig
      }
