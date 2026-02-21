;; Profit-Sharing Staking Pool Contract
;; Halal profit-sharing model - rewards come from actual revenue
;; Built by rajuice for Stacks Builder Rewards
;; Clarity 4 compatible (no as-contract)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-INSUFFICIENT-BALANCE (err u402))
(define-constant ERR-INVALID-AMOUNT (err u403))
(define-constant ERR-NO-STAKE (err u404))
(define-constant ERR-STAKE-LOCKED (err u405))
(define-constant ERR-POOL-EMPTY (err u406))

(define-constant MIN-STAKE u1000000) ;; Minimum 1 STX
(define-constant LOCK-PERIOD u144) ;; ~24 hours in blocks

(define-data-var total-staked uint u0)
(define-data-var profit-pool uint u0)
(define-data-var staking-enabled bool true)
(define-data-var total-stakers uint u0)
(define-data-var total-profit-distributed uint u0)

(define-map stakers principal {
  amount: uint,
  start-block: uint,
  profit-claimed: uint
})

;; Stake STX - funds sent to contract owner
(define-public (stake (amount uint))
  (let (
    (current-stake (default-to { amount: u0, start-block: u0, profit-claimed: u0 } (map-get? stakers tx-sender)))
    (new-amount (+ (get amount current-stake) amount))
  )
    (asserts! (var-get staking-enabled) ERR-NOT-AUTHORIZED)
    (asserts! (>= amount MIN-STAKE) ERR-INVALID-AMOUNT)
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (if (is-eq (get amount current-stake) u0)
      (var-set total-stakers (+ (var-get total-stakers) u1))
      true)
    (map-set stakers tx-sender {
      amount: new-amount,
      start-block: stacks-block-height,
      profit-claimed: (get profit-claimed current-stake)
    })
    (var-set total-staked (+ (var-get total-staked) amount))
    (ok new-amount)))

;; Owner returns staked STX
(define-public (process-unstake (staker principal) (amount uint))
  (let ((staker-info (unwrap! (map-get? stakers staker) ERR-NO-STAKE)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (>= (get amount staker-info) amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (>= stacks-block-height (+ (get start-block staker-info) LOCK-PERIOD)) ERR-STAKE-LOCKED)
    (try! (stx-transfer? amount tx-sender staker))
    (if (is-eq (get amount staker-info) amount)
      (begin
        (map-delete stakers staker)
        (var-set total-stakers (- (var-get total-stakers) u1)))
      (map-set stakers staker {
        amount: (- (get amount staker-info) amount),
        start-block: (get start-block staker-info),
        profit-claimed: (get profit-claimed staker-info)
      }))
    (var-set total-staked (- (var-get total-staked) amount))
    (ok amount)))

;; Request unstake (emits event for owner to process)
(define-public (request-unstake (amount uint))
  (let ((staker-info (unwrap! (map-get? stakers tx-sender) ERR-NO-STAKE)))
    (asserts! (>= (get amount staker-info) amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (>= stacks-block-height (+ (get start-block staker-info) LOCK-PERIOD)) ERR-STAKE-LOCKED)
    (print { event: "unstake-requested", staker: tx-sender, amount: amount })
    (ok true)))

;; Owner distributes profit to staker
(define-public (distribute-profit (staker principal) (amount uint))
  (let ((staker-info (unwrap! (map-get? stakers staker) ERR-NO-STAKE)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (>= (var-get profit-pool) amount) ERR-POOL-EMPTY)
    (try! (stx-transfer? amount tx-sender staker))
    (map-set stakers staker (merge staker-info { profit-claimed: (+ (get profit-claimed staker-info) amount) }))
    (var-set profit-pool (- (var-get profit-pool) amount))
    (var-set total-profit-distributed (+ (var-get total-profit-distributed) amount))
    (ok amount)))

;; Add revenue to profit pool
(define-public (add-profit (amount uint))
  (begin
    (var-set profit-pool (+ (var-get profit-pool) amount))
    (print { event: "profit-added", amount: amount, source: tx-sender })
    (ok (var-get profit-pool))))

;; Toggle staking
(define-public (toggle-staking)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set staking-enabled (not (var-get staking-enabled)))
    (ok (var-get staking-enabled))))

;; Calculate profit share
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

(define-read-only (get-staker-info (staker principal)) (map-get? stakers staker))
(define-read-only (get-total-staked) (ok (var-get total-staked)))
(define-read-only (get-profit-pool) (ok (var-get profit-pool)))
(define-read-only (get-total-stakers) (ok (var-get total-stakers)))
(define-read-only (is-staking-enabled) (ok (var-get staking-enabled)))
(define-read-only (get-min-stake) (ok MIN-STAKE))
(define-read-only (get-lock-period) (ok LOCK-PERIOD))
(define-read-only (get-total-profit-distributed) (ok (var-get total-profit-distributed)))
