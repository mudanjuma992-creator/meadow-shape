;; MeadowShape - Decentralized Ecosystem Gamification Platform

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-OWNER (err u100))
(define-constant ERR-NOT-AUTHORIZED (err u101))
(define-constant ERR-INVALID-BIOME (err u102))
(define-constant ERR-INVALID-SPECIES (err u103))
(define-constant ERR-ALREADY-STAKED (err u104))
(define-constant ERR-NOT-STAKED (err u105))
(define-constant ERR-INSUFFICIENT-BALANCE (err u106))
(define-constant ERR-INVALID-PROPOSAL (err u107))
(define-constant ERR-ALREADY-VOTED (err u108))
(define-constant ERR-PROPOSAL-CLOSED (err u109))
(define-constant ERR-TRANSFER-FAILED (err u110))
(define-constant ERR-INVALID-ORACLE (err u111))

;; Rarity tiers (based on conservation status)
(define-constant RARITY-COMMON u1)
(define-constant RARITY-UNCOMMON u2)
(define-constant RARITY-RARE u3)
(define-constant RARITY-ENDANGERED u4)
(define-constant RARITY-CRITICAL u5)

;; Biome evolution thresholds (eco-health score 0-100)
(define-constant BIOME-STAGE-SEEDLING u20)
(define-constant BIOME-STAGE-GROWING u40)
(define-constant BIOME-STAGE-THRIVING u60)
(define-constant BIOME-STAGE-FLOURISHING u80)
(define-constant BIOME-STAGE-PRISTINE u100)

;; Staking reward rates (tokens per block per staked biome)
(define-constant GROWTH-REWARD-RATE u10)
(define-constant IMPACT-REWARD-RATE u5)

;; Governance
(define-constant PROPOSAL-DURATION u1440) ;; ~10 days in blocks
(define-constant QUORUM-THRESHOLD u100)   ;; minimum governance tokens to pass

;; ============================================================
;; FUNGIBLE TOKENS
;; ============================================================

;; Growth Token - earned by staking biome NFTs
(define-fungible-token growth-token)

;; Impact Token - earned when real-world eco-health improves
(define-fungible-token impact-token)

;; Governance Token - used for DAO voting on conservation funding
(define-fungible-token governance-token)

;; ============================================================
;; NON-FUNGIBLE TOKENS
;; ============================================================

;; Biome NFT - dynamic, evolves based on environmental data
(define-non-fungible-token biome-nft uint)

;; Species NFT - native species obtained via Seed Packets
(define-non-fungible-token species-nft uint)

;; ============================================================
;; DATA MAPS & VARS
;; ============================================================

;; Global counters
(define-data-var biome-id-counter uint u0)
(define-data-var species-id-counter uint u0)
(define-data-var proposal-id-counter uint u0)
(define-data-var treasury-balance uint u0)

;; Authorized oracle addresses for environmental data feeds
(define-map authorized-oracles principal bool)

;; Biome NFT metadata
;; eco-health: 0-100 score from oracle
;; stage: 1-5 evolution stage
;; location-id: links to a real-world geographic region
(define-map biome-data
  uint
  {
    owner: principal,
    location-id: uint,
    eco-health: uint,
    stage: uint,
    last-updated: uint,
    is-staked: bool,
    stake-start-block: uint,
    accumulated-growth: uint,
    accumulated-impact: uint
  }
)

;; Species NFT metadata
;; rarity: 1-5 based on real conservation status
(define-map species-data
  uint
  {
    owner: principal,
    species-name: (string-ascii 64),
    rarity: uint,
    location-id: uint,
    minted-at: uint
  }
)

;; Environmental data per location (fed by oracles)
(define-map location-eco-data
  uint
  {
    eco-health: uint,
    last-updated: uint,
    oracle: principal
  }
)

;; Governance proposals for conservation funding
(define-map proposals
  uint
  {
    proposer: principal,
    description: (string-ascii 256),
    funding-amount: uint,
    recipient: principal,
    votes-for: uint,
    votes-against: uint,
    created-at: uint,
    is-executed: bool,
    is-active: bool
  }
)

;; Track votes per proposal per voter
(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  bool
)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

(define-private (get-biome-stage (eco-health uint))
  (if (>= eco-health BIOME-STAGE-PRISTINE)
    u5
    (if (>= eco-health BIOME-STAGE-FLOURISHING)
      u4
      (if (>= eco-health BIOME-STAGE-THRIVING)
        u3
        (if (>= eco-health BIOME-STAGE-GROWING)
          u2
          u1
        )
      )
    )
  )
)

(define-private (calculate-pending-growth (biome-id uint))
  (match (map-get? biome-data biome-id)
    biome
      (if (get is-staked biome)
        (let
          (
            (blocks-staked (- block-height (get stake-start-block biome)))
            (stage-multiplier (get stage biome))
          )
          (* (* blocks-staked GROWTH-REWARD-RATE) stage-multiplier)
        )
        u0
      )
    u0
  )
)

(define-private (calculate-pending-impact (biome-id uint))
  (match (map-get? biome-data biome-id)
    biome
      (if (get is-staked biome)
        (let
          (
            (blocks-staked (- block-height (get stake-start-block biome)))
            (eco-bonus (/ (get eco-health biome) u10))
          )
          (* (* blocks-staked IMPACT-REWARD-RATE) eco-bonus)
        )
        u0
      )
    u0
  )
)

;; Pseudo-random seed for loot box mechanics
;; Not cryptographically secure - suitable for low-stakes randomness
(define-private (pseudo-random (seed uint))
  (mod
    (+ (* seed u6364136223846793005) u1442695040888963407)
    u1000
  )
)

(define-private (determine-rarity (seed uint))
  (let ((roll (mod seed u100)))
    (if (< roll u5)
      RARITY-CRITICAL    ;; 5% chance
      (if (< roll u15)
        RARITY-ENDANGERED ;; 10% chance
        (if (< roll u35)
          RARITY-RARE      ;; 20% chance
          (if (< roll u60)
            RARITY-UNCOMMON  ;; 25% chance
            RARITY-COMMON    ;; 40% chance
          )
        )
      )
    )
  )
)

;; ============================================================
;; ORACLE FUNCTIONS
;; ============================================================

;; Owner authorizes trusted oracles
(define-public (authorize-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
    (map-set authorized-oracles oracle true)
    (ok true)
  )
)

(define-public (revoke-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
    (map-delete authorized-oracles oracle)
    (ok true)
  )
)

;; Oracle submits updated eco-health score for a location (0-100)
(define-public (update-location-eco-data
    (location-id uint)
    (eco-health uint))
  (begin
    (asserts!
      (default-to false (map-get? authorized-oracles tx-sender))
      ERR-INVALID-ORACLE
    )
    (asserts! (<= eco-health u100) ERR-INVALID-BIOME)
    (map-set location-eco-data location-id
      {
        eco-health: eco-health,
        last-updated: block-height,
        oracle: tx-sender
      }
    )
    (ok true)
  )
)

;; ============================================================
;; BIOME NFT FUNCTIONS
;; ============================================================

;; Mint a new Biome NFT for a given location
(define-public (mint-biome (location-id uint))
  (let
    (
      (new-id (+ (var-get biome-id-counter) u1))
      (location-data
        (default-to
          { eco-health: u50, last-updated: block-height, oracle: CONTRACT-OWNER }
          (map-get? location-eco-data location-id)
        )
      )
      (initial-health (get eco-health location-data))
      (initial-stage (get-biome-stage initial-health))
    )
    (try! (nft-mint? biome-nft new-id tx-sender))
    (map-set biome-data new-id
      {
        owner: tx-sender,
        location-id: location-id,
        eco-health: initial-health,
        stage: initial-stage,
        last-updated: block-height,
        is-staked: false,
        stake-start-block: u0,
        accumulated-growth: u0,
        accumulated-impact: u0
      }
    )
    (var-set biome-id-counter new-id)
    (ok new-id)
  )
)

;; Sync biome eco-health from its linked location oracle data
(define-public (evolve-biome (biome-id uint))
  (let
    (
      (biome (unwrap! (map-get? biome-data biome-id) ERR-INVALID-BIOME))
      (location-data
        (unwrap!
          (map-get? location-eco-data (get location-id biome))
          ERR-INVALID-BIOME
        )
      )
      (new-health (get eco-health location-data))
      (new-stage (get-biome-stage new-health))
      (pending-growth (calculate-pending-growth biome-id))
      (pending-impact (calculate-pending-impact biome-id))
    )
    (asserts! (is-eq tx-sender (get owner biome)) ERR-NOT-AUTHORIZED)
    (map-set biome-data biome-id
      (merge biome
        {
          eco-health: new-health,
          stage: new-stage,
          last-updated: block-height,
          accumulated-growth: (+ (get accumulated-growth biome) pending-growth),
          accumulated-impact: (+ (get accumulated-impact biome) pending-impact),
          stake-start-block: (if (get is-staked biome) block-height (get stake-start-block biome))
        }
      )
    )
    (ok { new-health: new-health, new-stage: new-stage })
  )
)

;; Transfer biome NFT ownership
(define-public (transfer-biome (biome-id uint) (recipient principal))
  (let ((biome (unwrap! (map-get? biome-data biome-id) ERR-INVALID-BIOME)))
    (asserts! (is-eq tx-sender (get owner biome)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get is-staked biome)) ERR-ALREADY-STAKED)
    (try! (nft-transfer? biome-nft biome-id tx-sender recipient))
    (map-set biome-data biome-id (merge biome { owner: recipient }))
    (ok true)
  )
)

;; ============================================================
;; SEED PACKET (LOOT BOX) FUNCTIONS
;; ============================================================

;; Open a Seed Packet to receive a randomized native species NFT
;; Costs 100 Growth Tokens per packet
(define-public (open-seed-packet
    (location-id uint)
    (species-name (string-ascii 64)))
  (let
    (
      (cost u100)
      (new-id (+ (var-get species-id-counter) u1))
      (seed (pseudo-random
        (+ block-height
           (+ (var-get species-id-counter) location-id))
      ))
      (rarity (determine-rarity seed))
    )
    (asserts!
      (>= (ft-get-balance growth-token tx-sender) cost)
      ERR-INSUFFICIENT-BALANCE
    )
    (try! (ft-burn? growth-token cost tx-sender))
    (try! (nft-mint? species-nft new-id tx-sender))
    (map-set species-data new-id
      {
        owner: tx-sender,
        species-name: species-name,
        rarity: rarity,
        location-id: location-id,
        minted-at: block-height
      }
    )
    (var-set species-id-counter new-id)
    (ok { species-id: new-id, rarity: rarity })
  )
)

;; Transfer species NFT
(define-public (transfer-species (species-id uint) (recipient principal))
  (let ((species (unwrap! (map-get? species-data species-id) ERR-INVALID-SPECIES)))
    (asserts! (is-eq tx-sender (get owner species)) ERR-NOT-AUTHORIZED)
    (try! (nft-transfer? species-nft species-id tx-sender recipient))
    (map-set species-data species-id (merge species { owner: recipient }))
    (ok true)
  )
)

;; ============================================================
;; STAKING FUNCTIONS
;; ============================================================

;; Stake a biome NFT to earn Growth and Impact tokens
(define-public (stake-biome (biome-id uint))
  (let ((biome (unwrap! (map-get? biome-data biome-id) ERR-INVALID-BIOME)))
    (asserts! (is-eq tx-sender (get owner biome)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get is-staked biome)) ERR-ALREADY-STAKED)
    (map-set biome-data biome-id
      (merge biome
        {
          is-staked: true,
          stake-start-block: block-height
        }
      )
    )
    (ok true)
  )
)

;; Unstake a biome NFT and claim pending token rewards
(define-public (unstake-biome (biome-id uint))
  (let
    (
      (biome (unwrap! (map-get? biome-data biome-id) ERR-INVALID-BIOME))
      (pending-growth (+ (get accumulated-growth biome) (calculate-pending-growth biome-id)))
      (pending-impact (+ (get accumulated-impact biome) (calculate-pending-impact biome-id)))
    )
    (asserts! (is-eq tx-sender (get owner biome)) ERR-NOT-AUTHORIZED)
    (asserts! (get is-staked biome) ERR-NOT-STAKED)
    ;; Mint rewards
    (try! (ft-mint? growth-token pending-growth tx-sender))
    (try! (ft-mint? impact-token pending-impact tx-sender))
    ;; Award governance tokens proportional to impact earned
    (try! (ft-mint? governance-token (/ pending-impact u10) tx-sender))
    ;; Update treasury with a portion of impact rewards
    (var-set treasury-balance (+ (var-get treasury-balance) (/ pending-impact u20)))
    ;; Reset staking state
    (map-set biome-data biome-id
      (merge biome
        {
          is-staked: false,
          stake-start-block: u0,
          accumulated-growth: u0,
          accumulated-impact: u0
        }
      )
    )
    (ok { growth-claimed: pending-growth, impact-claimed: pending-impact })
  )
)

;; Claim pending rewards without unstaking
(define-public (claim-rewards (biome-id uint))
  (let
    (
      (biome (unwrap! (map-get? biome-data biome-id) ERR-INVALID-BIOME))
      (pending-growth (+ (get accumulated-growth biome) (calculate-pending-growth biome-id)))
      (pending-impact (+ (get accumulated-impact biome) (calculate-pending-impact biome-id)))
    )
    (asserts! (is-eq tx-sender (get owner biome)) ERR-NOT-AUTHORIZED)
    (asserts! (get is-staked biome) ERR-NOT-STAKED)
    (try! (ft-mint? growth-token pending-growth tx-sender))
    (try! (ft-mint? impact-token pending-impact tx-sender))
    (try! (ft-mint? governance-token (/ pending-impact u10) tx-sender))
    (var-set treasury-balance (+ (var-get treasury-balance) (/ pending-impact u20)))
    ;; Reset accumulator and restart staking epoch
    (map-set biome-data biome-id
      (merge biome
        {
          stake-start-block: block-height,
          accumulated-growth: u0,
          accumulated-impact: u0
        }
      )
    )
    (ok { growth-claimed: pending-growth, impact-claimed: pending-impact })
  )
)

;; ============================================================
;; GOVERNANCE (DAO) FUNCTIONS
;; ============================================================

;; Submit a conservation funding proposal
(define-public (create-proposal
    (description (string-ascii 256))
    (funding-amount uint)
    (recipient principal))
  (let
    (
      (new-id (+ (var-get proposal-id-counter) u1))
      (gov-balance (ft-get-balance governance-token tx-sender))
    )
    ;; Must hold at least 10 governance tokens to propose
    (asserts! (>= gov-balance u10) ERR-NOT-AUTHORIZED)
    (asserts! (<= funding-amount (var-get treasury-balance)) ERR-INSUFFICIENT-BALANCE)
    (map-set proposals new-id
      {
        proposer: tx-sender,
        description: description,
        funding-amount: funding-amount,
        recipient: recipient,
        votes-for: u0,
        votes-against: u0,
        created-at: block-height,
        is-executed: false,
        is-active: true
      }
    )
    (var-set proposal-id-counter new-id)
    (ok new-id)
  )
)

;; Vote on a conservation funding proposal
;; vote-for: true = support, false = oppose
(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) ERR-INVALID-PROPOSAL))
      (voter-balance (ft-get-balance governance-token tx-sender))
      (vote-key { proposal-id: proposal-id, voter: tx-sender })
    )
    (asserts! (get is-active proposal) ERR-PROPOSAL-CLOSED)
    (asserts!
      (<= block-height (+ (get created-at proposal) PROPOSAL-DURATION))
      ERR-PROPOSAL-CLOSED
    )
    (asserts! (is-none (map-get? proposal-votes vote-key)) ERR-ALREADY-VOTED)
    (asserts! (> voter-balance u0) ERR-INSUFFICIENT-BALANCE)
    (map-set proposal-votes vote-key true)
    (map-set proposals proposal-id
      (merge proposal
        {
          votes-for: (if vote-for
            (+ (get votes-for proposal) voter-balance)
            (get votes-for proposal)
          ),
          votes-against: (if vote-for
            (get votes-against proposal)
            (+ (get votes-against proposal) voter-balance)
          )
        }
      )
    )
    (ok true)
  )
)

;; Execute a passed proposal and release conservation funds
(define-public (execute-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-INVALID-PROPOSAL)))
    (asserts! (get is-active proposal) ERR-PROPOSAL-CLOSED)
    (asserts! (not (get is-executed proposal)) ERR-PROPOSAL-CLOSED)
    (asserts!
      (> block-height (+ (get created-at proposal) PROPOSAL-DURATION))
      ERR-PROPOSAL-CLOSED
    )
    ;; Proposal passes if votes-for exceeds votes-against and meets quorum
    (asserts!
      (and
        (> (get votes-for proposal) (get votes-against proposal))
        (>= (get votes-for proposal) QUORUM-THRESHOLD)
      )
      ERR-NOT-AUTHORIZED
    )
    (asserts!
      (<= (get funding-amount proposal) (var-get treasury-balance))
      ERR-INSUFFICIENT-BALANCE
    )
    ;; Release impact tokens to conservation project recipient
    (try! (ft-mint? impact-token (get funding-amount proposal) (get recipient proposal)))
    (var-set treasury-balance (- (var-get treasury-balance) (get funding-amount proposal)))
    (map-set proposals proposal-id
      (merge proposal { is-executed: true, is-active: false })
    )
    (ok true)
  )
)

;; ============================================================
;; READ-ONLY FUNCTIONS
;; ============================================================

(define-read-only (get-biome (biome-id uint))
  (map-get? biome-data biome-id)
)

(define-read-only (get-species (species-id uint))
  (map-get? species-data species-id)
)

(define-read-only (get-location-data (location-id uint))
  (map-get? location-eco-data location-id)
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-treasury-balance)
  (var-get treasury-balance)
)

(define-read-only (get-growth-balance (account principal))
  (ft-get-balance growth-token account)
)

(define-read-only (get-impact-balance (account principal))
  (ft-get-balance impact-token account)
)

(define-read-only (get-governance-balance (account principal))
  (ft-get-balance governance-token account)
)

(define-read-only (get-pending-rewards (biome-id uint))
  (match (map-get? biome-data biome-id)
    biome
      (ok {
        pending-growth: (+ (get accumulated-growth biome) (calculate-pending-growth biome-id)),
        pending-impact: (+ (get accumulated-impact biome) (calculate-pending-impact biome-id))
      })
    ERR-INVALID-BIOME
  )
)

(define-read-only (is-oracle-authorized (oracle principal))
  (default-to false (map-get? authorized-oracles oracle))
)

(define-read-only (get-biome-count)
  (var-get biome-id-counter)
)

(define-read-only (get-species-count)
  (var-get species-id-counter)
)

(define-read-only (get-proposal-count)
  (var-get proposal-id-counter)
)
