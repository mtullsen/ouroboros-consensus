{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-strictness #-}

module Ouroboros.Consensus.HardFork.Combinator.Node () where

import           Data.Proxy
import           Data.SOP.Strict
import           GHC.Stack
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Forging ()
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.CommonProtocolParams ()
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.PeerSelection ()
import           Ouroboros.Consensus.HardFork.Combinator.Node.InitStorage ()
import           Ouroboros.Consensus.HardFork.Combinator.Node.Metrics ()
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run

{-------------------------------------------------------------------------------
  ConfigSupportsNode
-------------------------------------------------------------------------------}

instance CanHardFork xs => ConfigSupportsNode (HardForkBlock xs) where
  getSystemStart  = getSameConfigValue getSystemStart
  getNetworkMagic = getSameConfigValue getNetworkMagic

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getSameConfigValue
  :: forall xs a. (CanHardFork xs, Eq a, HasCallStack)
  => (forall blk. ConfigSupportsNode blk => BlockConfig blk -> a)
  -> BlockConfig (HardForkBlock xs)
  -> a
getSameConfigValue getValue blockConfig = getSameValue values
  where
    values :: NP (K a) xs
    values =
          hcmap (Proxy @SingleEraBlock) (K . getValue)
        . getPerEraBlockConfig
        . hardForkBlockConfigPerEra
        $ blockConfig

{-------------------------------------------------------------------------------
  RunNode
-------------------------------------------------------------------------------}

instance ( CanHardFork xs
           -- Instances that must be defined for specific values of @b@:
         , LedgerTablesCanHardFork xs
         , LedgerSupportsMempool (HardForkBlock xs)
         , SupportedNetworkProtocolVersion (HardForkBlock xs)
         , SerialiseHFC xs
         ) => RunNode (HardForkBlock xs)
