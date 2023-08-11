{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.SyncTest (tests) where

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
import           Data.Foldable (for_)
import           Data.Functor
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust, isNothing)
import           Data.Monoid (Endo (Endo), First (..), appEndo)
import           Debug.Trace (trace)
import           Network.TypedProtocol.Channel (createConnectedChannels)
import           Network.TypedProtocol.Driver.Simple
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB.Impl
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (Fingerprint (..),
                     WithFingerprint (..), blockUntilChanged)
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
import qualified Test.Ouroboros.Consensus.ChainGenerator.Adversarial
import           Test.Ouroboros.Consensus.ChainGenerator.Counting
                     (Count (Count))
import           Test.Ouroboros.Consensus.ChainGenerator.Honest
                     (HonestRecipe (HonestRecipe))
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Asc (Asc),
                     Kcp (Kcp), Len (Len), Scg (Scg))
import qualified Test.Ouroboros.Consensus.ChainGenerator.Tests.Adversarial
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.Adversarial
                     (SomeTestAdversarial (SomeTestAdversarial),
                     TestAdversarial)
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.ChainDb
                     (computeHeaderStateHistory, computePastLedger)
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.GenChain
                     (genChains)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
import           Test.QuickCheck.Random (QCGen)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.ChainDB
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock

tests :: TestTree
tests = testGroup "SyncingTest"
    [ testProperty "basic" prop_syncGenesis
    ]

exampleTestSetup ::
  TestAdversarial base hon ->
  QCGen ->
  TestSetup
exampleTestSetup params seed =
  trace ("ASC: " ++ show ascH) $
  trace ("k: " ++ show k) $
  trace ("chain growth: " ++ show scg) $
  trace ("good: " ++ condense goodChain) $
  trace ("bad: " ++ condense badChain) $
  TestSetup {
    secParam      = SecurityParam (fromIntegral k)
  , genesisWindow = GenesisWindow (fromIntegral scg)
  , ..
  }
  where
    Asc ascH = params.testAscH
    HonestRecipe (Kcp k) (Scg scg) _ (Len len) = params.testRecipeH
    genesisAcrossIntersection = not genesisAfterIntersection && len > scg
    -- TODO: also need: at least k+1 blocks after the intersection
    genesisAfterIntersection =
         fragLenA > fromIntegral scg
      && advLenAfterIntersection > fromIntegral k
    advLenAfterIntersection =
      withOrigin 0 unBlockNo (AF.headBlockNo badChain) - unSlotNo prefixLen
    (goodChain, badChain, prefixLen, fragLenA) = genChains params.testAscA params.testRecipeA params.testRecipeA' seed

prop_syncGenesis :: SomeTestAdversarial -> QCGen -> QC.Property
prop_syncGenesis (SomeTestAdversarial _ _ params) seed =
  runSimOrThrow $ runTest (exampleTestSetup params seed)

newtype GenesisWindow = GenesisWindow { getGenesisWindow :: SlotNo }
  deriving stock (Show)

data TestSetup = TestSetup {
    secParam                  :: SecurityParam
  , genesisWindow             :: GenesisWindow
  , goodChain                 :: AnchoredFragment TestBlock
  , badChain                  :: AnchoredFragment TestBlock
  , genesisAcrossIntersection :: Bool -- rename to isLongRangeAttack?
  , genesisAfterIntersection  :: Bool
  }
  deriving stock (Show)

runTest ::
     forall m. IOLike m
  => TestSetup
  -> m Property
runTest TestSetup{..} = withRegistry \registry -> do
    varGoodCandidate <- uncheckedNewTVarM $ AF.Empty AF.AnchorGenesis
    varBadCandidate  <- uncheckedNewTVarM $ AF.Empty AF.AnchorGenesis
    chainDB <-
      let monitor = [("Good", varGoodCandidate), ("Bad", varBadCandidate)]
      in  mkRealChainDb monitor registry
    let chainDbView = defaultChainDbView chainDB
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
    finalSelection     <- atomically $ ChainDB.getCurrentChain chainDB
    let finalSelectionIntersectsGood =
          isJust (AF.intersect goodChain (AF.mapAnchoredFragment testHeader finalSelection))
    pure
      $ classify genesisAfterIntersection "Long range attack"
      $ classify genesisAcrossIntersection "Genesis potential"
      $ counterexample ("result: " <> show res)
      $ counterexample ("final good candidate: " <> condense finalGoodCandidate)
      $ counterexample ("final bad candidate: " <> condense finalBadCandidate)
      $ counterexample ("final selection: " <> condense finalSelection)
      $ counterexample ("genesisAfterIntersection: " <> show genesisAfterIntersection)
      -- $ expectFailure
      $ conjoin [
          property $ res == Right ()
        , property $ isLongRangeAttack == not finalSelectionIntersectsGood
        ]
  where
    isLongRangeAttack = genesisAfterIntersection

    implies a b = not a || b

    SecurityParam k = secParam

    checkIntersection = \case
      Nothing -> counterexample "No intersection" (reverseCondition False)
      Just frag -> validIntersection frag

    validIntersection int = appEndo (foldMap (Endo . counterexample) intersectionDesc) (property (reverseCondition (intersectionProp int)))

    intersectionDesc
      | genesisAfterIntersection = ["Long range attack"]
      | genesisAcrossIntersection = ["Genesis potential"]
      | otherwise = ["Partial Genesis"]

    intersectionProp int
      -- If there is a full genesis window after the intersection, the selected fragment should be longer than k blocks.
      | genesisAfterIntersection = not (forksWithinK int)
      | genesisAcrossIntersection = forksWithinK int
      | otherwise = False

    forksWithinK
      :: (AnchoredFragment (Header TestBlock), AnchoredFragment (Header TestBlock), AnchoredFragment (Header TestBlock), AnchoredFragment (Header TestBlock))
      -> Bool
    forksWithinK (_ourPrefix, _theirPrefix, ourSuffix, _theirSuffix) =
      fromIntegral (AF.length ourSuffix) <= k

    reverseCondition = not

    mkRealChainDb ::
         [(String, StrictTVar m (AnchoredFragment (Header TestBlock)))]
      -> ResourceRegistry m
      -> m (ChainDB m TestBlock)
    mkRealChainDb candidateVars registry = do
        chainDbArgs <- do
          mcdbNodeDBs <- emptyNodeDBs
          pure $ fromMinimalChainDbArgs MinimalChainDbArgs {
              mcdbTopLevelConfig = nodeCfg
            , mcdbChunkInfo      = mkTestChunkInfo nodeCfg
            , mcdbInitLedger     = testInitExtLedger
            , mcdbRegistry       = registry
            , mcdbNodeDBs
            }
        (_, (chainDB, ChainDB.Impl.Internal{intAddBlockRunner})) <-
          allocate
            registry
            (\_ -> ChainDB.Impl.openDBInternal chainDbArgs False)
            (ChainDB.closeDB . fst)
        _ <- forkLinkedThread registry "AddBlockRunner" intAddBlockRunner
        for_ candidateVars \(name, varCandidate) ->
          forkLinkedThread registry name $ monitorCandidate chainDB varCandidate
        pure chainDB
      where
        monitorCandidate chainDB varCandidate =
            go GenesisPoint
          where
            go candidateTip = do
              ((frag, candidateTip'), isFetched) <- atomically $
                (,)
                  <$> blockUntilChanged AF.headPoint candidateTip (readTVar varCandidate)
                  <*> ChainDB.getIsFetched chainDB
              let blks =
                      filter (not . isFetched . blockPoint)
                    $ testHeader <$> AF.toOldestFirst frag
              for_ blks $ ChainDB.addBlock_ chainDB InvalidBlockPunishment.noPunishment
              go candidateTip'

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
