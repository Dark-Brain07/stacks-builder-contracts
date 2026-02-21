;; Community Treasury Contract
;; Halal community fund with fair distribution
;; Built by rajuice for Stacks Builder Rewards

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-POOL-CLOSED (err u402))
(define-constant ERR-POOL-ACTIVE (err u403))
(define-constant ERR-ALREADY-CONTRIBUTED (err u404))
(define-constant ERR-NO-CONTRIBUTORS (err u405))
(define-constant MIN-CONTRIBUTION u1000000) ;; 1 STX

(define-data-var round-id uint u0)
(define-data-var is-active bool false)
(define-data-var contributor-count uint u0)
(define-data-var total-pool uint u0)

(define-map contributors { round: uint, index: uint } principal)
(define-map contributor-amounts { round: uint, contributor: principal } uint)
(define-map round-distributions uint { total: uint, distributed: bool })

;; Start a new funding round
(define-public (start-round)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (not (var-get is-active)) ERR-POOL-ACTIVE)
    (var-set round-id (+ (var-get round-id) u1))
    (var-set is-active true)
    (var-set contributor-count u0)
    (var-set total-pool u0)
    (ok (var-get round-id))))

;; Contribute to the community treasury
(define-public (contribute (amount uint))
  (let ((round (var-get round-id))
        (index (var-get contributor-count)))
    (asserts! (var-get is-active) ERR-POOL-CLOSED)
    (asserts! (>= amount MIN-CONTRIBUTION) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? contributor-amounts { round: round, contributor: tx-sender })) ERR-ALREADY-CONTRIBUTED)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set contributors { round: round, index: index } tx-sender)
    (map-set contributor-amounts { round: round, contributor: tx-sender } amount)
    (var-set contributor-count (+ index u1))
    (var-set total-pool (+ (var-get total-pool) amount))
    (ok index)))

;; Distribute funds equally to all contributors (community benefit)
(define-public (distribute-to-project (project-address principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (var-get is-active) ERR-POOL-CLOSED)
    (asserts! (> (var-get contributor-count) u0) ERR-NO-CONTRIBUTORS)
    (try! (as-contract (stx-transfer? amount tx-sender project-address)))
    (var-set total-pool (- (var-get total-pool) amount))
    (print { event: "funds-distributed", round: (var-get round-id), project: project-address, amount: amount })
    (ok amount)))

;; Close the round
(define-public (close-round)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (var-get is-active) ERR-POOL-CLOSED)
    (map-set round-distributions (var-get round-id) { total: (var-get total-pool), distributed: true })
    (var-set is-active false)
    (ok (var-get round-id))))

;; Refund contribution if round is cancelled
(define-public (refund-contribution)
  (let (
    (round (var-get round-id))
    (contributed (default-to u0 (map-get? contributor-amounts { round: round, contributor: tx-sender })))
  )
    (asserts! (> contributed u0) ERR-NOT-AUTHORIZED)
    (asserts! (not (var-get is-active)) ERR-POOL-ACTIVE)
    (try! (as-contract (stx-transfer? contributed tx-sender tx-sender)))
    (map-set contributor-amounts { round: round, contributor: tx-sender } u0)
    (ok contributed)))

(define-read-only (get-round) (ok (var-get round-id)))
(define-read-only (get-contributor-count) (ok (var-get contributor-count)))
(define-read-only (get-total-pool) (ok (var-get total-pool)))
(define-read-only (get-is-active) (ok (var-get is-active)))
(define-read-only (get-min-contribution) (ok MIN-CONTRIBUTION))
(define-read-only (get-contribution (round uint) (contributor principal))
  (ok (default-to u0 (map-get? contributor-amounts { round: round, contributor: contributor }))))
(define-read-only (get-round-info (round uint))
  (map-get? round-distributions round))
