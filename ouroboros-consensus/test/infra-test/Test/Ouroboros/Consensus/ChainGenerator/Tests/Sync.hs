{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.Sync where

import Cardano.Crypto.DSIGN (SignKeyDSIGN (..), VerKeyDSIGN (..))
import Cardano.Slotting.Slot (SlotNo)
import Cardano.Slotting.Time (SlotLength, slotLengthFromSec)
import Control.Concurrent.Class.MonadSTM (MonadSTM (TChan, newTChanIO, writeTChan), readTChan)
import Control.Monad (forever)
import Control.Monad.Class.MonadTime (MonadTime, getCurrentTime)
import Control.Monad.State.Strict (StateT, evalStateT, get, gets, lift, modify')
import Control.Tracer (nullTracer)
import Data.Foldable (for_, traverse_)
import Data.Functor (void, (<&>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Data.Time (DiffTime, NominalDiffTime, UTCTime, addUTCTime)
import Network.TypedProtocol.Channel (createConnectedChannels)
import Network.TypedProtocol.Driver.Simple (runConnectedPeersPipelined)
import Ouroboros.Consensus.Block.Abstract (GetHeader (getHeader), Header, Point)
import Ouroboros.Consensus.Config (SecurityParam, TopLevelConfig (..))
import qualified Ouroboros.Consensus.HardFork.History.EraParams as HardFork (EraParams, defaultEraParams)
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client (
  ChainDbView,
  ChainSyncClientException,
  Consensus,
  chainSyncClient,
  )
import Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (NumCoreNodes))
import Ouroboros.Consensus.NodeId (CoreNodeId (CoreNodeId), NodeId (CoreId))
import Ouroboros.Consensus.Protocol.BFT (
  BftParams (BftParams, bftNumNodes, bftSecurityParam),
  ConsensusConfig (BftConfig, bftParams, bftSignKey, bftVerKeys),
  )
import Ouroboros.Consensus.Util.IOLike (
  IOLike,
  MonadAsync (async, waitCatch),
  MonadDelay (threadDelay),
  MonadSTM (TQueue, newTQueueIO),
  StrictTVar,
  atomically,
  stateTVar,
  try,
  uncheckedNewTVarM, cancel, race,
  )
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import Ouroboros.Network.Block (Tip (..))
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined (ChainSyncClientPipelined, chainSyncClientPeerPipelined)
import Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSyncId)
import Ouroboros.Network.Protocol.ChainSync.PipelineDecision (pipelineDecisionLowHighMark)
import Ouroboros.Network.Protocol.ChainSync.Server (
  ChainSyncServer (..),
  ServerStIdle (ServerStIdle, recvMsgDoneClient, recvMsgFindIntersect, recvMsgRequestNext),
  ServerStIntersect (SendMsgIntersectFound, SendMsgIntersectNotFound),
  ServerStNext (SendMsgRollForward),
  chainSyncServerPeer,
  )
import Test.Util.Orphans.IOLike ()
import Test.Util.TestBlock (
  BlockConfig (TestBlockConfig),
  CodecConfig (TestBlockCodecConfig),
  StorageConfig (TestBlockStorageConfig),
  TestBlock,
  )

type BlockServer blk m = ChainSyncServer (Header blk) (Point blk) (Tip blk) m ()

data Peer m =
  Peer {
    wait :: m (),
    kill :: m (),
    events :: TQueue m String
  }

data GlobalEvent = EventStart

data SyncTest m =
  SyncTest {
    topConfig :: TopLevelConfig TestBlock,
    peers :: [Peer m],
    globalEvents :: TChan m GlobalEvent
  }

defaultCfg :: SecurityParam -> TopLevelConfig TestBlock
defaultCfg secParam = TopLevelConfig {
    topLevelConfigProtocol = BftConfig {
      bftParams  = BftParams {
        bftSecurityParam = secParam
      , bftNumNodes      = NumCoreNodes 2
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
  where
    slotLength :: SlotLength
    slotLength = slotLengthFromSec 20

    eraParams :: HardFork.EraParams
    eraParams = HardFork.defaultEraParams secParam slotLength

    numCoreNodes = NumCoreNodes 2

data Triggered c b = Triggered c b

data Condition =
  Started
  |
  Immediate
  |
  AtSlot SlotNo
  |
  ElapsedTime NominalDiffTime

awaitCondition ::
  IOLike m =>
  TChan m GlobalEvent ->
  Condition ->
  BlockServer TestBlock m ->
  m (BlockServer TestBlock m)
awaitCondition ev Started s =
  atomically (readTChan ev) >>= \ EventStart -> pure s
awaitCondition _ _ s =
  pure s

data ChainMsg m =
  ChainMsg {
    intersect :: ([Point TestBlock] -> m (Tip TestBlock, Maybe (AnchoredFragment TestBlock))),
    next :: (m (Tip TestBlock, Maybe TestBlock))
  }

data ServerBehavior m =
  ServeChainHonest (ChainMsg m)
  |
  ServeChainDelayed DiffTime (ChainMsg m)
  |
  ServerTimeout

data Server m =
  Server {
    candidate :: StrictTVar m (AnchoredFragment (Header TestBlock)),
    behaviors :: NonEmpty (Triggered Condition (ServerBehavior m))
  }

mkServer ::
  IOLike m =>
  ServerBehavior m ->
  [Triggered Condition (ServerBehavior m)] ->
  m (Server m)
mkServer initial rest = do
  candidate <- uncheckedNewTVarM $ AF.Empty AF.AnchorGenesis
  pure Server {behaviors = Triggered Started initial :| rest, ..}

data ActiveServer m =
  ActiveServer {
    startTime :: UTCTime,
    currentBehavior :: ServerBehavior m,
    nextBehaviors :: [Triggered Condition (ServerBehavior m)]
  }

type TestServer m = BlockServer TestBlock m

withBehavior ::
  IOLike m =>
  MonadTime m =>
  StrictTVar m (ActiveServer m) ->
  (ServerBehavior m -> m a) ->
  m a
withBehavior state use = do
  now <- getCurrentTime
  b <- atomically $ stateTVar state \case
    old@ActiveServer {startTime, currentBehavior, nextBehaviors = Triggered (ElapsedTime diff) next : rest}
      | now > addUTCTime diff startTime ->
        (next, ActiveServer {startTime, currentBehavior = next, nextBehaviors = rest})
      | otherwise ->
        (currentBehavior, old)
    ActiveServer {nextBehaviors = Triggered Immediate next : rest, ..} ->
      (next, ActiveServer {currentBehavior = next, nextBehaviors = rest, ..})
    old@ActiveServer {currentBehavior} ->
      (currentBehavior, old)
  use b

runTestServer ::
  IOLike m =>
  MonadTime m =>
  StrictTVar m (ActiveServer m) ->
  TestServer m
runTestServer state =
  go
  where
    go = ChainSyncServer $ pure ServerStIdle {
        recvMsgRequestNext = withBehavior state \case
          ServeChainHonest cm -> requestNext cm
          ServeChainDelayed delay cm -> threadDelay delay >> requestNext cm
          ServerTimeout -> stall
      , recvMsgFindIntersect = \pts -> withBehavior state \case
          ServeChainHonest cm -> findIntersect pts cm
          ServeChainDelayed delay cm -> threadDelay delay >> findIntersect pts cm
          ServerTimeout -> stall
      , recvMsgDoneClient = pure ()
    }

    requestNext (ChainMsg _ next) =
      next >>= \case
        (_, Nothing) -> pure $ Right stall
        (tip, Just blk) -> pure (Left $ SendMsgRollForward (getHeader blk) tip go)

    findIntersect pts (ChainMsg intersect _) =
      intersect pts <&> \case
        (tip, Just frag') -> SendMsgIntersectFound (AF.anchorPoint frag') tip go
        (tip, Nothing) -> SendMsgIntersectNotFound tip go

    stall = forever $ threadDelay 1000

runServer ::
  IOLike m =>
  MonadTime m =>
  TChan m GlobalEvent ->
  Server m ->
  m (BlockServer TestBlock m)
runServer ev Server {behaviors = Triggered cond behav :| rest} = do
  now <- getCurrentTime
  act <- uncheckedNewTVarM (ActiveServer now behav rest)
  awaitCondition ev cond (runTestServer act)

basicChainSyncClient ::
  IOLike m =>
  TopLevelConfig TestBlock ->
  ChainDbView m TestBlock ->
  StrictTVar m (AnchoredFragment (Header TestBlock)) ->
  Consensus ChainSyncClientPipelined TestBlock m
basicChainSyncClient cfg chainDbView varCandidate =
  chainSyncClient
    (pipelineDecisionLowHighMark 10 20)
    nullTracer
    cfg
    chainDbView
    maxBound
    (return Continue)
    nullTracer
    varCandidate

syncWith ::
  IOLike m =>
  MonadTime m =>
  ChainDbView m TestBlock ->
  Server m ->
  StateT (SyncTest m) m ()
syncWith chainDbView server = do
  cfg <- gets topConfig
  ev <- gets globalEvents
  events <- lift newTQueueIO
  handle <- lift $ async do
    s <- runServer ev server
    runConnectedPeersPipelined
      createConnectedChannels
      nullTracer
      codecChainSyncId
      (chainSyncClientPeerPipelined (basicChainSyncClient cfg chainDbView (candidate server)))
      (chainSyncServerPeer s)
  let wait = void (waitCatch handle)
      kill = cancel handle
  modify' \ SyncTest {..} -> SyncTest {peers = Peer {..} : peers, ..}

awaitAll ::
  IOLike m =>
  SyncTest m ->
  m ()
awaitAll SyncTest {..} =
  void $
  race (threadDelay 10) $
  for_ peers \ Peer {..} -> wait

syncTest ::
  IOLike m =>
  SecurityParam ->
  StateT (SyncTest m) m a ->
  m (Either ChainSyncClientException a)
syncTest k setup = do
  globalEvents <- newTChanIO
  flip evalStateT SyncTest {topConfig = defaultCfg k, peers = [], globalEvents} do
    a <- setup
    lift (atomically (writeTChan globalEvents EventStart))
    s <- get
    res <- lift (try (awaitAll s))
    pure (a <$ res)

syncPeers ::
  IOLike m =>
  MonadTime m =>
  (TopLevelConfig TestBlock -> ChainDbView m TestBlock) ->
  SecurityParam ->
  [Server m] ->
  m (Either ChainSyncClientException ())
syncPeers mkDbView k peers =
  syncTest k do
    cfg <- gets topConfig
    let chainDbView = mkDbView cfg
    traverse_ (syncWith chainDbView) peers
