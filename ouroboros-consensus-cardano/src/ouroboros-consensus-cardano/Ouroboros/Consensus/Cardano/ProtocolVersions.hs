{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

module Ouroboros.Consensus.Cardano.ProtocolVersions (
    -- * Cardano protocol versions
    CardanoProtocolVersions (CardanoProtocolVersions, protVerByron, protVerShelley, protVerAllegra, protVerMary, protVerAlonzo, protVerBabbage, protVerConway)
  , cardanoMainnetMaxProtocolVersion
  , cardanoMainnetProtocolVersions
    -- * Protocol version type family
  , ProtocolVersion
  ) where

import qualified Cardano.Chain.Update as Byron.Update
import qualified Cardano.Ledger.BaseTypes as SL (natVersion)
import qualified Cardano.Ledger.Shelley.API as SL
import           Data.Kind (Type)
import           Data.SOP.NonEmpty (IsNonEmpty (..), ProofEmpty (ProofEmpty),
                     ProofNonEmpty (ProofNonEmpty), checkEmptiness)
import           Data.SOP.Strict (NP (..), Proxy (..), SListI)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Cardano.Block (CardanoEras)
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)

{-------------------------------------------------------------------------------
  Cardano protocol versions
-------------------------------------------------------------------------------}

newtype CardanoProtocolVersions c = CardanoProtocolVersions_ {
    getCardanoProtocolVersions :: PerEraProtocolVersion (CardanoEras c)
  }

pattern CardanoProtocolVersions ::
     ProtocolVersion ByronBlock
  -> ProtocolVersion (ShelleyBlock (TPraos c) (ShelleyEra c))
  -> ProtocolVersion (ShelleyBlock (TPraos c) (AllegraEra c))
  -> ProtocolVersion (ShelleyBlock (TPraos c) (MaryEra    c))
  -> ProtocolVersion (ShelleyBlock (TPraos c) (AlonzoEra  c))
  -> ProtocolVersion (ShelleyBlock (Praos  c) (BabbageEra c))
  -> ProtocolVersion (ShelleyBlock (Praos  c) (ConwayEra  c))
  -> CardanoProtocolVersions c
pattern CardanoProtocolVersions {
      protVerByron
    , protVerShelley
    , protVerAllegra
    , protVerMary
    , protVerAlonzo
    , protVerBabbage
    , protVerConway
    } =
  CardanoProtocolVersions_ {
      getCardanoProtocolVersions = PerEraProtocolVersion {
          getPerEraProtocolVersion =
              WrapProtocolVersion protVerByron
            :* WrapProtocolVersion protVerShelley
            :* WrapProtocolVersion protVerAllegra
            :* WrapProtocolVersion protVerMary
            :* WrapProtocolVersion protVerAlonzo
            :* WrapProtocolVersion protVerBabbage
            :* WrapProtocolVersion protVerConway
            :* Nil
        }
    }

cardanoMainnetProtocolVersions :: CardanoProtocolVersions c
cardanoMainnetProtocolVersions = CardanoProtocolVersions {
      protVerByron   = Byron.Update.ProtocolVersion 1 2 0
    , protVerShelley = SL.ProtVer (SL.natVersion @3) 0
    , protVerAllegra = SL.ProtVer (SL.natVersion @4) 0
    , protVerMary    = SL.ProtVer (SL.natVersion @5) 0
    , protVerAlonzo  = SL.ProtVer (SL.natVersion @7) 0
    , protVerBabbage = SL.ProtVer (SL.natVersion @9) 0
    , protVerConway  = SL.ProtVer (SL.natVersion @9) 0
    }

cardanoMainnetMaxProtocolVersion :: CardanoProtocolVersions c -> SL.ProtVer
cardanoMainnetMaxProtocolVersion = maxProtocolVersion . getCardanoProtocolVersions

{-------------------------------------------------------------------------------
  Protocol version type family
-------------------------------------------------------------------------------}

type ProtocolVersion :: Type -> Type
type family ProtocolVersion blk where
  ProtocolVersion ByronBlock               = Byron.Update.ProtocolVersion
  ProtocolVersion (ShelleyBlock proto era) = SL.ProtVer

{-------------------------------------------------------------------------------
  Internal: value for /each/ era
-------------------------------------------------------------------------------}

type PerEraProtocolVersion :: [Type] -> Type
newtype PerEraProtocolVersion xs = PerEraProtocolVersion {
    getPerEraProtocolVersion :: NP WrapProtocolVersion xs
  }

type WrapProtocolVersion :: Type -> Type
newtype WrapProtocolVersion blk = WrapProtocolVersion {
    unwrapProtocolVersion :: ProtocolVersion blk
  }

{-------------------------------------------------------------------------------
  Internal: maximum protocol version
-------------------------------------------------------------------------------}

maxProtocolVersion ::
     (IsNonEmpty xs, SListI xs)
  => PerEraProtocolVersion xs
  -> ProtocolVersion (Last xs)
maxProtocolVersion = unwrapProtocolVersion . lastNP . getPerEraProtocolVersion

type Last :: [k] -> k
type family Last xs where
  Last '[x]    = x
  Last (x:xs) = Last xs

lastNP :: forall f xs. (IsNonEmpty xs, SListI xs) => NP f xs -> f (Last xs)
lastNP xs0 =
    case isNonEmpty (Proxy @xs) of
      ProofNonEmpty Proxy (Proxy :: Proxy xs') ->
        case checkEmptiness (Proxy @xs') of
          Left ProofEmpty ->
            case xs0 of
              x :* Nil -> x
          Right (ProofNonEmpty Proxy (Proxy :: Proxy xs'')) ->
            case xs0 of
              _ :* xs -> lastNP xs


