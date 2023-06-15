{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

-- | Queries to the mempool
module Ouroboros.Consensus.Mempool.Query (
    implGetSnapshotFor
  , pureGetSnapshotFor
  ) where

import           Data.Foldable (foldl')
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.Tables.Utils (emptyLedgerTables)
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.Capacity
import           Ouroboros.Consensus.Mempool.Impl.Common
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.ReadsKeySets
import           Ouroboros.Consensus.Util.IOLike

implGetSnapshotFor ::
     ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     )
  => MempoolEnv m blk
  -> SlotNo -- ^ Get snapshot for this slot number (usually the current slot)
  -> TickedLedgerState blk DiffMK -- ^ The ledger state at 'pt' ticked to 'slot'
  -> LedgerTables (ExtLedgerState blk) SeqDiffMK
  -> LedgerBackingStoreValueHandle' m blk
  -> m (MempoolSnapshot blk)
implGetSnapshotFor mpEnv slot ticked extChlog extLbsvh = do
  is <- atomically $ readTMVar istate
  if pointHash (isTip is) == castHash (getTipHash ticked) &&
     isSlotNo is == slot
    then
      -- We are looking for a snapshot exactly for the ledger state we already
      -- have cached, then just return it.
      pure . snapshotFromIS $ is
    else do
       let keys = foldl' (<>) emptyLedgerTables
                $ map getTransactionKeySets
                $ [ txForgetValidated . TxSeq.txTicketTx $ tx
                  | tx <- TxSeq.toList $ isTxs is
                  ]
       values <- bsvhRead vh keys
       let eTbs = forwardTableKeySets' bsvhAtSlot chlog $ UnforwardedReadSets {
                   ursSeqNo  = bsvhAtSlot
                 , ursValues = values
                 , ursKeys   = keys
                 }
       pure $ getSnap is $ case eTbs of
         Right tbs -> tbs
         Left e -> error $ "Critical error, value handle and changelog \
                           \should be in the same slot thanks to the RAWLock. \
                           \Seeing this means the RAWLock has failed! " <> show e
  where
    vh@BackingStoreValueHandle { bsvhAtSlot } =
      castBackingStoreValueHandle
         unExtLedgerStateTables
         ExtLedgerStateTables
         extLbsvh

    chlog = unExtLedgerStateTables extChlog

    getSnap is tbs = pureGetSnapshotFor
                       capacityOverride
                       cfg
                       tbs
                       is
                       (ForgeInKnownSlot slot ticked)
    MempoolEnv { mpEnvStateVar         = istate
               , mpEnvLedgerCfg        = cfg
               , mpEnvCapacityOverride = capacityOverride
               } = mpEnv

-- | Get a snapshot of the mempool state that is valid with respect to
-- the given ledger state, together with the ticked ledger state.
pureGetSnapshotFor
  :: ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     )
  => MempoolCapacityBytesOverride
  -> LedgerConfig blk
  -> LedgerTables (LedgerState blk) ValuesMK
  -> InternalState blk
  -> ForgeLedgerState blk
  -> MempoolSnapshot blk
pureGetSnapshotFor _ _ _ _ ForgeInUnknownSlot{} =
  error "Tried to get a snapshot for unknown slot"
pureGetSnapshotFor capacityOverride cfg values is (ForgeInKnownSlot slot st) =
  snapshotFromIS $
    if pointHash (isTip is) == castHash (getTipHash st) && isSlotNo is == slot
    then is
    else fst $ revalidateTxsFor
                 capacityOverride
                 cfg
                 slot
                 st
                 values
                 (isLastTicketNo is)
                 (TxSeq.toList $ isTxs is)
