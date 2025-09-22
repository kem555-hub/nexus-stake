;; NexusStake - Decentralized Perpetual Futures Protocol
;; Simplified implementation focusing on core staking and yield distribution

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-not-found (err u103))
(define-constant err-already-staked (err u104))
(define-constant err-cooldown-active (err u105))

;; Data Variables
(define-data-var total-staked uint u0)
(define-data-var total-yield-distributed uint u0)
(define-data-var protocol-fee uint u250) ;; 2.5% in basis points
(define-data-var min-stake-amount uint u1000000) ;; 1 STX minimum
(define-data-var cooldown-period uint u144) ;; ~1 day in blocks

;; Data Maps
(define-map stakers 
  principal 
  {
    amount: uint,
    stake-time: uint,
    last-yield-claim: uint,
    risk-score: uint
  }
)

(define-map vault-balances
  {asset: (string-ascii 10)}
  {
    total-amount: uint,
    yield-rate: uint,
    last-rebalance: uint
  }
)

(define-map governance-proposals
  uint
  {
    proposer: principal,
    title: (string-ascii 50),
    votes-for: uint,
    votes-against: uint,
    end-block: uint,
    executed: bool
  }
)

(define-data-var proposal-counter uint u0)

;; Read-only functions

(define-read-only (get-staker-info (staker principal))
  (map-get? stakers staker)
)

(define-read-only (get-total-staked)
  (var-get total-staked)
)

(define-read-only (get-vault-balance (asset (string-ascii 10)))
  (map-get? vault-balances {asset: asset})
)

(define-read-only (calculate-yield (staker principal))
  (let
    (
      (staker-info (unwrap! (map-get? stakers staker) u0))
      (stake-amount (get amount staker-info))
      (risk-score (get risk-score staker-info))
      (blocks-staked (- block-height (get stake-time staker-info)))
      (base-yield (/ (* stake-amount blocks-staked) u1000))
      (risk-multiplier (+ u100 risk-score)) ;; 1.0 + risk bonus
    )
    (/ (* base-yield risk-multiplier) u100)
  )
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? governance-proposals proposal-id)
)

;; Private functions

(define-private (calculate-risk-score (amount uint))
  (if (> amount u10000000) ;; 10 STX
    u50 ;; 0.5x multiplier for large stakes
    u25 ;; 0.25x multiplier for smaller stakes
  )
)

(define-private (update-vault-yield (asset (string-ascii 10)) (new-rate uint))
  (map-set vault-balances 
    {asset: asset}
    (merge 
      (default-to 
        {total-amount: u0, yield-rate: u0, last-rebalance: u0}
        (map-get? vault-balances {asset: asset})
      )
      {yield-rate: new-rate, last-rebalance: block-height}
    )
  )
)

;; Public functions

(define-public (stake-tokens (amount uint))
  (let
    (
      (sender tx-sender)
      (current-balance (stx-get-balance sender))
      (risk-score (calculate-risk-score amount))
    )
    (asserts! (>= amount (var-get min-stake-amount)) err-invalid-amount)
    (asserts! (>= current-balance amount) err-insufficient-balance)
    (asserts! (is-none (map-get? stakers sender)) err-already-staked)
    
    (try! (stx-transfer? amount sender (as-contract tx-sender)))
    
    (map-set stakers sender {
      amount: amount,
      stake-time: block-height,
      last-yield-claim: block-height,
      risk-score: risk-score
    })
    
    (var-set total-staked (+ (var-get total-staked) amount))
    
    ;; Update STX vault balance
    (update-vault-yield "STX" u500) ;; 5% base yield
    
    (ok true)
  )
)

(define-public (unstake-tokens)
  (let
    (
      (sender tx-sender)
      (staker-info (unwrap! (map-get? stakers sender) err-not-found))
      (stake-amount (get amount staker-info))
      (stake-time (get stake-time staker-info))
    )
    (asserts! (>= (- block-height stake-time) (var-get cooldown-period)) err-cooldown-active)
    
    (try! (as-contract (stx-transfer? stake-amount tx-sender sender)))
    
    (map-delete stakers sender)
    (var-set total-staked (- (var-get total-staked) stake-amount))
    
    (ok stake-amount)
  )
)

(define-public (claim-yield)
  (let
    (
      (sender tx-sender)
      (staker-info (unwrap! (map-get? stakers sender) err-not-found))
      (yield-amount (calculate-yield sender))
      (protocol-fee-amount (/ (* yield-amount (var-get protocol-fee)) u10000))
      (net-yield (- yield-amount protocol-fee-amount))
    )
    (asserts! (> yield-amount u0) err-invalid-amount)
    
    ;; Update last claim time
    (map-set stakers sender 
      (merge staker-info {last-yield-claim: block-height})
    )
    
    ;; Mint yield tokens (simplified as STX transfer)
    (try! (as-contract (stx-transfer? net-yield tx-sender sender)))
    
    (var-set total-yield-distributed (+ (var-get total-yield-distributed) yield-amount))
    
    (ok net-yield)
  )
)

(define-public (rebalance-vault (asset (string-ascii 10)) (new-yield-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (update-vault-yield asset new-yield-rate)
    (ok true)
  )
)

(define-public (create-proposal (title (string-ascii 50)))
  (let
    (
      (proposal-id (+ (var-get proposal-counter) u1))
      (sender tx-sender)
    )
    (asserts! (is-some (map-get? stakers sender)) err-not-found)
    
    (map-set governance-proposals proposal-id {
      proposer: sender,
      title: title,
      votes-for: u0,
      votes-against: u0,
      end-block: (+ block-height u1008), ;; ~1 week voting period
      executed: false
    })
    
    (var-set proposal-counter proposal-id)
    (ok proposal-id)
  )
)

(define-public (vote-proposal (proposal-id uint) (vote-for bool))
  (let
    (
      (sender tx-sender)
      (staker-info (unwrap! (map-get? stakers sender) err-not-found))
      (proposal (unwrap! (map-get? governance-proposals proposal-id) err-not-found))
      (voting-power (get amount staker-info))
    )
    (asserts! (< block-height (get end-block proposal)) err-invalid-amount)
    (asserts! (not (get executed proposal)) err-invalid-amount)
    
    (if vote-for
      (map-set governance-proposals proposal-id
        (merge proposal {votes-for: (+ (get votes-for proposal) voting-power)})
      )
      (map-set governance-proposals proposal-id
        (merge proposal {votes-against: (+ (get votes-against proposal) voting-power)})
      )
    )
    
    (ok true)
  )
)

;; Admin functions

(define-public (set-protocol-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u1000) err-invalid-amount) ;; Max 10%
    (var-set protocol-fee new-fee)
    (ok true)
  )
)

(define-public (set-min-stake-amount (new-amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set min-stake-amount new-amount)
    (ok true)
  )
)

(define-public (emergency-pause)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    ;; In a full implementation, this would set a pause flag
    (ok true)
  )
)

;; Initialize default vault
(update-vault-yield "STX" u500)