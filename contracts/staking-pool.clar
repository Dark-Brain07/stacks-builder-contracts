;; Profit-Sharing Staking Pool Contract
;; Halal profit-sharing model - rewards come from actual revenue, not fixed interest
;; Built by rajuice for Stacks Builder Rewards

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-INSUFFICIENT-BALANCE (err u402))
(define-constant ERR-INVALID-AMOUNT (err u403))
(define-constant ERR-NO-STAKE (err u404))
(define-constant ERR-STAKE-LOCKED (err u405))
(define-constant ERR-POOL-EMPTY (err u406))

;; Staking parameters
(define-constant MIN-STAKE u1000000) ;; Minimum 1 STX
(define-constant LOCK-PERIOD u144) ;; ~24 hours in blocks

;; Data vars
(define-data-var total-staked uint u0)
(define-data-var profit-pool uint u0) ;; Revenue from dApp fees, services, etc.
(define-data-var staking-enabled bool true)
(define-data-var total-stakers uint u0)
(define-data-var total-profit-distributed uint u0)

;; Data maps
(define-map stakers principal {
  amount: uint,
  start-block: uint,
  profit-claimed: uint
})

;; Private function to get contract principal
(define-private (get-contract-principal)
  (as-contract tx-sender))

;; Staking Functions

;; Stake STX tokens (profit-sharing, NOT interest-based)
(define-public (stake (amount uint))
  (let (
    (current-stake (default-to { amount: u0, start-block: u0, profit-claimed: u0 } (map-get? stakers tx-sender)))
    (new-amount (+ (get amount current-stake) amount))
    (contract-address (get-contract-principal))
  )
    (asserts! (var-get staking-enabled) ERR-NOT-AUTHORIZED)
    (asserts! (>= amount MIN-STAKE) ERR-INVALID-AMOUNT)
    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender contract-address))
    ;; Update staker info
    (if (is-eq (get amount current-stake) u0)
      (var-set total-stakers (+ (var-get total-stakers) u1))
      true
    )
    (map-set stakers tx-sender {
      amount: new-amount,
      start-block: stacks-block-height,
      profit-claimed: (get profit-claimed current-stake)
    })
    ;; Update total staked
    (var-set total-staked (+ (var-get total-staked) amount))
    (ok new-amount)))

;; Private function for withdrawing funds
(define-private (withdraw-stx (amount uint) (recipient principal))
  (as-contract (stx-transfer? amount tx-sender recipient)))

;; Unstake tokens
(define-public (unstake (amount uint))
  (let (
    (staker-info (unwrap! (map-get? stakers tx-sender) ERR-NO-STAKE))
    (staked-amount (get amount staker-info))
    (start-block (get start-block staker-info))
    (caller tx-sender)
  )
    (asserts! (>= staked-amount amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (>= stacks-block-height (+ start-block LOCK-PERIOD)) ERR-STAKE-LOCKED)
    ;; Transfer STX back to user
    (try! (withdraw-stx amount caller))
    ;; Update staker info
    (if (is-eq staked-amount amount)
      (begin
        (map-delete stakers caller)
        (var-set total-stakers (- (var-get total-stakers) u1)))
      (map-set stakers caller {
        amount: (- staked-amount amount),
        start-block: start-block,
        profit-claimed: (get profit-claimed staker-info)
      }))
    ;; Update total staked
    (var-set total-staked (- (var-get total-staked) amount))
    (ok amount)))

;; Claim profit share (proportional to stake amount)
(define-public (claim-profit)
  (let (
    (staker-info (unwrap! (map-get? stakers tx-sender) ERR-NO-STAKE))
    (share (calculate-profit-share tx-sender))
    (caller tx-sender)
  )
    (asserts! (> share u0) ERR-INSUFFICIENT-BALANCE)
    (asserts! (>= (var-get profit-pool) share) ERR-POOL-EMPTY)
    ;; Transfer profit share
    (try! (withdraw-stx share caller))
    ;; Update claimed
    (map-set stakers caller {
      amount: (get amount staker-info),
      start-block: (get start-block staker-info),
      profit-claimed: (+ (get profit-claimed staker-info) share)
    })
    ;; Update profit pool
    (var-set profit-pool (- (var-get profit-pool) share))
    (var-set total-profit-distributed (+ (var-get total-profit-distributed) share))
    (ok share)))

;; Calculate profit share based on proportion of stake
;; This is halal: rewards come from ACTUAL revenue, distributed proportionally
(define-read-only (calculate-profit-share (staker principal))
  (let (
    (staker-info (default-to { amount: u0, start-block: u0, profit-claimed: u0 } (map-get? stakers staker)))
    (staked-amount (get amount staker-info))
    (pool (var-get profit-pool))
    (total (var-get total-staked))
  )
    (if (and (> staked-amount u0) (> total u0) (> pool u0))
      (/ (* pool staked-amount) total)
      u0)))

;; Admin Functions

;; Add revenue to profit pool (from dApp fees, services, etc.)
;; This is the KEY halal difference: profits come from real business activity
(define-public (add-profit (amount uint))
  (let ((contract-address (get-contract-principal)))
    (try! (stx-transfer? amount tx-sender contract-address))
    (var-set profit-pool (+ (var-get profit-pool) amount))
    (print { event: "profit-added", amount: amount, source: tx-sender })
    (ok (var-get profit-pool))))

;; Toggle staking (owner only)
(define-public (toggle-staking)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set staking-enabled (not (var-get staking-enabled)))
    (ok (var-get staking-enabled))))

;; Emergency withdraw (owner only)
(define-public (emergency-withdraw)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (let ((balance (stx-get-balance (get-contract-principal))))
      (try! (withdraw-stx balance CONTRACT-OWNER))
      (var-set total-staked u0)
      (var-set profit-pool u0)
      (ok balance))))

;; Read-only Functions

(define-read-only (get-staker-info (staker principal))
  (map-get? stakers staker))

(define-read-only (get-total-staked)
  (ok (var-get total-staked)))

(define-read-only (get-profit-pool)
  (ok (var-get profit-pool)))

(define-read-only (get-total-stakers)
  (ok (var-get total-stakers)))

(define-read-only (is-staking-enabled)
  (ok (var-get staking-enabled)))

(define-read-only (get-min-stake)
  (ok MIN-STAKE))

(define-read-only (get-lock-period)
  (ok LOCK-PERIOD))

(define-read-only (get-total-profit-distributed)
  (ok (var-get total-profit-distributed)))

(define-read-only (get-unlock-block (staker principal))
  (let ((staker-info (map-get? stakers staker)))
    (match staker-info
      info (ok (+ (get start-block info) LOCK-PERIOD))
      (err ERR-NO-STAKE))))
