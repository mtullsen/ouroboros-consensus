# Ouroboros-consensus Cardano Changelog

# Changelog entries

<a id='changelog-0.9.0.0'></a>
## 0.9.0.0 — 2023-09-06

### Non-Breaking

- Updated to newer `cardano-ledger-*` packages:
    * `cardano-ledger-api-1.5`,
    * `cardano-ledger-alonzo-1.4.1`
    * `cardano-ledger-shelley-1.6`
    * `cardano-ledger-babbage-1.4.4`
    * `cardano-ledger-conway-1.8`
    * `cardano-ledger-tpraos-1.0.3.5`

### Breaking

- Use `ouroboros-network-framework-0.8`. Types of
  `Cardano.Tools.ImmDBServer.MiniProtocols.immDBServer` and
  `Cardano.Tools.ImmDBServer.Diffusion.serve` have changed.

<a id='changelog-0.8.0.1'></a>
## 0.8.0.1 — 2023-08-21

### Patch

- Removed the `expose-sublibs` cabal flag, since Cabal/Nix handled it poorly.
- Instead, added a `unstable-` prefix to the name of each sublibrary, to
  strongly indicate that we ignore them when evolving the package's version.

<a id='changelog-0.8.0.0'></a>
## 0.8.0.0 — 2023-08-18

### Patch

- Update `fs-api` dependency to `^>=0.2`

### Non-Breaking

- Add new `ProtocolInfo` module to `cardano-testlib`, containing utilities for
  creating Cardano `ProtocolInfo`s for testing purposes.

- Expose the latest Conway ledger queries.
    - `GetCommitteeState`
    - `GetConstitution`
    - `GetDRepStakeDistr setOfDReps`
    - `GetDRepState setOfDRepCredentials`
    - `GetGovState`

- Add a `ProtocolTransitionParams` data family, and provide instances for
  transitions from Byron to Shelley and Shelley-based eras to Shelley-based
  eras.
- Add a data instance of `ProtocolParams` for the Cardano block.
- Provide a `CardanoProtocolParams` type synonym and associated pattern synonym
  (with record syntax).
- Export all `ProtocolParams` and `ProtocolTransitionParams` instances from
  `Ouroboros.Consensus.Cardano` and `Ouroboros.Consensus.Cardano.Node`.

### Breaking

- Update ledger dependencies to pull in the latest Conway changes.
    - `cardano-ledger-conway` from `^>=1.6` to `^>=1.7`
    - `cardano-ledger-alonzo` from `^>=1.3` to `^>=1.4`
    - `cardano-ledger-api` from `^>=1.3` to `^>=1.4`
    - `cardano-ledger-core` from `^>=1.4` to `^>=1.5`
    - `cardano-ledger-shelley` from `^>=1.4.1` to `^>=1.5`
- Remove the `degenerateAlonzoGenesis` declaration.
- Delete the (as of yet unreleased) `GetConstitutionHash`.
    - Use `anchorDataHash . constitutionAnchor <$> GetConstitution` instead.

- Refactor `ProtocolParamsByron` to a data instance of `ProtocolParams`.
- Refactor protocol parameters for Shelley eras (e.g, `ProtocolParamsAlonzo` and `ProtocolParamsBabbage`) to data instances of `ProtocolParams`.
- Export all Shelley `ProtocolParams` data instances from `Ouroboros.Consensus.Shelley.Node`.
- Remove the `ProtocolTransitionParamsShelleyBased` datatype in favour of
  `ProtocolTransitionParams`.
- Make `protocolInfoCardano` require a `CardanoProtocolParams` type as its only
  argument, instead of a long list of arguments.

<a id='changelog-0.7.0.0'></a>
## 0.7.0.0 — 2023-07-06

### Non-Breaking

- Refactor code because block forging credentials got extracted out of
  `ProtocolInfo` type.

### Breaking

- Change the return type of numerous functions to include block forging credentials since
  they got extracted out of `ProtocolInfo` type.
  - Refactor the type signatures to accommodate the fact that `ProtocolInfo` does not
  need monad type variable.

- Add `GetConstitutionHash` ledger query

<a id='changelog-0.6.1.0'></a>
## 0.6.1.0 — 2023-06-23

### Patch

- Rename `StrictMVar` to `StrictSVar`

- Add support for new `cardano-ledger` package versions

### Non-Breaking

- Call `cryptoInit` before running test suites

- Make sure `defaultMainWithTestEnv` is used everywhere

- Call `cryptoInit` in `defaultMainWithTestEnv`

- Call `cryptoInit` in our utility tools

<a id='changelog-0.6.0.0'></a>
## 0.6.0.0 — 2023-05-19

### Patch

- Optimise `GetStakeSnapshots` query to not traverse all delegations
  per stake pool, but instead compute the total stake per pool in a
  map and do a lookup
- Update CHaPs dependencies
- Fix performance regression of `GetFilteredDelegationsAndRewardAccounts` query
- Remove deprecated pattern synonyms from `Ouroboros.Consensus.Shelley.Ledger`:
  `GetUTxO` and `GetFilteredUTxO`.

### Breaking

- Bumped latest released node versions to `NodeToNodeV_11` and `NodeToClientV_15`.
- `NodeToClientV_15` enables the deposits query.
- The `GetCurrentPParams` query now uses the legacy en-/decoding for its result again when the `NodeToClientVersion` is `<15`, restoring compatibility with older clients.

### Non-Breaking

- Bump `cardano-ledger-{alonzo,babbage}` to 1.2.1.0, which changes the corresponding `PParams` serialisation. This affects the ledger snapshots, and the `GetCurrentPParams` query for `NodeToClientVersion >= 15`.

<a id='changelog-0.5.0.1'></a>
## 0.5.0.1 — 2023-04-28

### Patch

- Update `ouroboros-network` dependency.

<a id='changelog-0.5.0.0'></a>
## 0.5.0.0 - 2023-04-24

### Breaking

- Apply new organization of Consensus packages. Absorb `byron`, `shelley`,
  `cardano-tools` and all the testing packages for those.

- Add the new `ShelleyNodeToClientVersion7` to the `ShelleyNodeToClientVersion`

### Non-Breaking

- Add a new ledger query: `GetStakeDelegDeposits`

<a id='changelog-0.4.0.1'></a>
## 0.4.0.1 — 2023-04-10

### Patch

- `ouroboros-consensus-cardano`: Since the filesystem API that lives in
  `ouroboros-consensus` will live in the `fs-api` package for now on, start
  depending on `fs-api`, and change imports accordingly.

- Collapse all imports into one group in every file.
- Adapt to relocation of SOP-related `Util` modules.

<a id='changelog-0.4.0.0'></a>
## 0.4.0.0 — 2023-03-07

### Non-Breaking

- Fix the version bounds for the bundle and version sets the bounds for the
  `ouroboros-consensus` bundle to `^>=0.3`.

### Breaking

- Return stake snapshots for stake pools that have the `Set` or `Go` ledger
  snapshots. When querying `GetStakeSnapshots Nothing`, which means to query for
  all stake pools, previously only stake snapshots for stake pools that have the
  `Mark` ledger snapshot were returned.

<a id='changelog-0.3.0.0'></a>
## 0.3.0.0 — 2023-02-09

### Patch

- Remove redundant proxy argument for `ledgerDbTip`.

### Non-Breaking

- Adapted to the new reorganization of Mempool modules in `ouroboros-consensus`.

### Breaking

####  Added:
- `Conway` to `CardanoEras`
- `NodeToNodeV_11` and `NodeToClientV_15`, both of which enable Conway.
- Conway-related type and pattern synonyms. Eg `StandardConway`, `HeaderConway`,
  `GentTxConway`, etc.

#### Changed

- The `protocolInfoTPraosShelleyBased` and `protocolInfoPraosShelleyBased`
  functions now expect a tuple of the `AdditionalGenesisConfig` and the
  `TranslationContext` instead of just the `TranslationContext`. For all
  Shelley-based eras before Conway, those had been equal types.

---

### Archaeological remark

Before following a more structured release process, we tracked most significant
changes affecting downstream users in the
[interface-CHANGELOG.md](https://github.com/input-output-hk/ouroboros-consensus/blob/8d8329e4dd41404439b7cd30629fcce427679212/docs/website/docs/interface-CHANGELOG.md).
