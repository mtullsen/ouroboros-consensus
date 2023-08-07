module Test.Ouroboros.Consensus.ChainGenerator.Tests.PointSchedule (
  module Test.Ouroboros.Consensus.ChainGenerator.Tests.PointSchedule,
) where

import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import Ouroboros.Network.Block (Tip)
import Test.Util.TestBlock (Header, TestBlock)

data AdvertisedPoints =
  AdvertisedPoints {
    tip :: Tip TestBlock,
    header :: Header TestBlock,
    block :: TestBlock,
    fragment :: AnchoredFragment TestBlock
  }
  deriving (Eq, Show)

data NodeState =
  NodeOnline AdvertisedPoints
  |
  NodeOffline
  deriving (Eq, Show)

data PointTick =
  PointTick {
    number :: Int,
    state :: NodeState
  }
  deriving (Eq, Show)

data PointSchedule =
  PointSchedule [PointTick]
  deriving (Eq, Show)

generatePointSchedule ::
  AnchoredFragment TestBlock ->
  AnchoredFragment TestBlock ->
  (PointSchedule, PointSchedule)
generatePointSchedule =
  undefined
