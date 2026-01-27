;; Staking Pool Contract
;; Stake STX tokens and earn rewards
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
(define-constant REWARD-RATE u100) ;; 1% rewards per period (100 basis points)

;; Data vars
(define-data-var total-staked uint u0)
(define-data-var reward-pool uint u0)
(define-data-var staking-enabled bool true)
(define-data-var total-stakers uint u0)

;; Data maps
(define-map stakers principal {
  amount: uint,
  start-block: uint,
  rewards-claimed: uint
})

;; Private function to get contract principal
(define-private (get-contract-principal)
  (as-contract tx-sender))

;; Staking Functions

;; Stake STX tokens
(define-public (stake (amount uint))
  (let (
    (current-stake (default-to { amount: u0, start-block: u0, rewards-claimed: u0 } (map-get? stakers tx-sender)))
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
      start-block: block-height,
      rewards-claimed: (get rewards-claimed current-stake)
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
    (asserts! (>= block-height (+ start-block LOCK-PERIOD)) ERR-STAKE-LOCKED)
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
        rewards-claimed: (get rewards-claimed staker-info)
      }))
    ;; Update total staked
    (var-set total-staked (- (var-get total-staked) amount))
    (ok amount)))

;; Claim rewards
(define-public (claim-rewards)
  (let (
    (staker-info (unwrap! (map-get? stakers tx-sender) ERR-NO-STAKE))
    (pending-rewards (calculate-pending-rewards tx-sender))
    (caller tx-sender)
  )
    (asserts! (> pending-rewards u0) ERR-INSUFFICIENT-BALANCE)
    (asserts! (>= (var-get reward-pool) pending-rewards) ERR-POOL-EMPTY)
    ;; Transfer rewards
    (try! (withdraw-stx pending-rewards caller))
    ;; Update claimed rewards
    (map-set stakers caller {
      amount: (get amount staker-info),
      start-block: (get start-block staker-info),
      rewards-claimed: (+ (get rewards-claimed staker-info) pending-rewards)
    })
    ;; Update reward pool
    (var-set reward-pool (- (var-get reward-pool) pending-rewards))
    (ok pending-rewards)))

;; Compound rewards (claim and restake)
(define-public (compound)
  (let (
    (staker-info (unwrap! (map-get? stakers tx-sender) ERR-NO-STAKE))
    (pending-rewards (calculate-pending-rewards tx-sender))
    (new-amount (+ (get amount staker-info) pending-rewards))
  )
    (asserts! (> pending-rewards u0) ERR-INSUFFICIENT-BALANCE)
    (asserts! (>= (var-get reward-pool) pending-rewards) ERR-POOL-EMPTY)
    ;; Update staker with compounded amount
    (map-set stakers tx-sender {
      amount: new-amount,
      start-block: block-height,
      rewards-claimed: (+ (get rewards-claimed staker-info) pending-rewards)
    })
    ;; Update totals
    (var-set total-staked (+ (var-get total-staked) pending-rewards))
    (var-set reward-pool (- (var-get reward-pool) pending-rewards))
    (ok new-amount)))

;; Helper function to calculate pending rewards
(define-read-only (calculate-pending-rewards (staker principal))
  (let (
    (staker-info (default-to { amount: u0, start-block: u0, rewards-claimed: u0 } (map-get? stakers staker)))
    (staked-amount (get amount staker-info))
    (start-block (get start-block staker-info))
    (blocks-staked (- block-height start-block))
    (periods-completed (/ blocks-staked LOCK-PERIOD))
  )
    (if (> staked-amount u0)
      (/ (* staked-amount (* REWARD-RATE periods-completed)) u10000)
      u0)))

;; Admin Functions

;; Add to reward pool
(define-public (add-rewards (amount uint))
  (let ((contract-address (get-contract-principal)))
    (try! (stx-transfer? amount tx-sender contract-address))
    (var-set reward-pool (+ (var-get reward-pool) amount))
    (ok (var-get reward-pool))))

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
      (var-set reward-pool u0)
      (ok balance))))

;; Read-only Functions

(define-read-only (get-staker-info (staker principal))
  (map-get? stakers staker))

(define-read-only (get-total-staked)
  (ok (var-get total-staked)))

(define-read-only (get-reward-pool)
  (ok (var-get reward-pool)))

(define-read-only (get-total-stakers)
  (ok (var-get total-stakers)))

(define-read-only (is-staking-enabled)
  (ok (var-get staking-enabled)))

(define-read-only (get-min-stake)
  (ok MIN-STAKE))

(define-read-only (get-lock-period)
  (ok LOCK-PERIOD))

(define-read-only (get-reward-rate)
  (ok REWARD-RATE))

(define-read-only (get-unlock-block (staker principal))
  (let ((staker-info (map-get? stakers staker)))
    (match staker-info
      info (ok (+ (get start-block info) LOCK-PERIOD))
      (err ERR-NO-STAKE))))
