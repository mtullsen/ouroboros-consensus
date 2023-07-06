# Introduction

We are currently implementing a small refinement to the syncing node.
The Genesis release will supersede this, but this can be released sooner.

# Desired High-Level Behavior

Desideratum 1.
The following state machine depicts the desired behavior of the node.

```mermaid

graph    
    OnlyBootstrap[OnlyBootstrap]
    CaughtUp[CaughtUp]

    OnlyBootstrap -- "no peers claim to have\nsubsequent headers" --> CaughtUp
    CaughtUp -- "vol tip became older than X" --> OnlyBootstrap

    StartUp[[Node start-up]]
    StartUp -- "node was most recently in CaughtUp\nand vol tip is younger than X" --> CaughtUp
    StartUp -- "otherwise" --> OnlyBootstrap
```

- `OnlyBootstrap` state - All upstream peers must reside in a centralized set of trusted _bootstrap peers_.

- `CaughtUp` state - The peers are chosen according to the P2P design, including the _ledger peers_ etc.

Desideratum 2.
In particular, the transitions should happen promptly.

- `CaughtUp -> OnlyBootstrap` should be prompt in order to minimize the duration that the node is exposed to untrusted peers (aka non-bootstrap peers) while its stale volatile tip is making it vulnerable.
  Delays here would directly threaten the security of the node.

- `OnlyBootstrap -> CaughtUp` should be prompt so that the centralized, relatively-few bootstrap peers are relieved of load as soon as possible.
  Delays here would no directly threaten the security of the node.
  However, wasting the centralized resources would threaten the ability of nodes to join the net, ie the availability of the whole net.

Desideratum 3.
The node should not return to `OnlyBootstrap` every time it restarts/briefly loses network/etc.
Such unnecessary connections would also put unnecessary load on the centralized, relatively-few bootstrap peers.
This is the point of the "Node start-up" psuedo state in the diagram above.

# Information Exchange Requirements

- When in `OnlyBootstrap`, the Diffusion Layer needs to know the addresses of the bootstrap peers.
  This data will likely come from configuration files, and so doesn't require communication between the Diffusion Layer and the Consensus Layer.

- When in `CaughtUp`, the Diffusion Layer needs to know the addresses and weights of the registered stake pool relays---aka the stake distribution among the ledger peers.
  This information is in the ledger state, and so the Consensus Layer will need to provide to the Diffusion Layer continually.

- For the sake of efficient reuse, the Diffusion Layer must manage the disconnections from all peers upon the `CaughtUp -> OnlyBootstrap` transition.
  It would be wasteful for the Consensus Layer to achieve this less carefully, eg by simply killing those threads.

  Therefore, the Consensus Layer needs to promptly notify the Diffusion Layer when the node should make the `CaughtUp -> OnlyBootstrap` transition.
  Ideally, this happens immediately, so the implementation will likely involve an `STM` `TVar`.

  As the volatile tip age approaches X, the Consensus Layer could forewarn the Diffusion Layer, eg "it seems like the transition back to OnlyBootstrap will be necessary soon; please prepare", if that would be helpful.

- For similar reasons, the Diffusion Layer should also manage the disconections from all peers upon the `OnlyBootstrap -> CaughtUp` transition.

# Anticipated Interface

- Whenever necessary, the Diffusion Layer can ask the Consensus Layer for the ledger peer information.
  EG `lpGetPeers :: STM m (SlotNo, [(PoolStake, NonEmpty RelayAccessPoint)])`.

- The Diffusion Layer should monitor a `TVar State` (maybe via a `STM State` action).
  The Consensus Layer will update that state promptly.
  The Diffusion Layer should react promptly.
  EG either the existing `FetchMode` or else a new sibling `TVar`.
