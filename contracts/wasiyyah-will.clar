;; Wasiyyah Will Contract (Islamic Will)
;; Record and execute Islamic wills
;; Halal - fulfills wasiyyah obligation
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-EXECUTED (err u405))
(define-constant ERR-NOT-EXECUTOR (err u406))
(define-constant MAX-BEQUEST-PCT u33) ;; Maximum 1/3 for non-heirs per Islamic law

(define-data-var will-count uint u0)

(define-map wills uint {
  testator: principal, executor: principal, total-estate: uint,
  status: (string-ascii 20), created: uint, executed-at: uint
})
(define-map bequests { will-id: uint, index: uint } { beneficiary: principal, share-pct: uint, amount: uint, distributed: bool })
(define-map bequest-count uint uint)
(define-map testator-will principal uint)

(define-public (create-will (executor principal) (estate-value uint))
  (let ((id (+ (var-get will-count) u1)))
    (map-set wills id { testator: tx-sender, executor: executor, total-estate: estate-value, status: "active", created: stacks-block-height, executed-at: u0 })
    (map-set testator-will tx-sender id)
    (map-set bequest-count id u0)
    (var-set will-count id) (ok id)))

(define-public (add-bequest (will-id uint) (beneficiary principal) (share-pct uint))
  (let (
    (will (unwrap! (map-get? wills will-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? bequest-count will-id)))
    (amount (/ (* (get total-estate will) share-pct) u100))
  )
    (asserts! (is-eq tx-sender (get testator will)) ERR-NOT-AUTHORIZED)
    (map-set bequests { will-id: will-id, index: idx } { beneficiary: beneficiary, share-pct: share-pct, amount: amount, distributed: false })
    (map-set bequest-count will-id (+ idx u1)) (ok idx)))

(define-public (execute-bequest (will-id uint) (bequest-index uint))
  (let (
    (will (unwrap! (map-get? wills will-id) ERR-NOT-FOUND))
    (beq (unwrap! (map-get? bequests { will-id: will-id, index: bequest-index }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get executor will)) ERR-NOT-EXECUTOR)
    (asserts! (not (is-eq (get status will) "revoked")) ERR-ALREADY-EXECUTED)
    (asserts! (not (get distributed beq)) ERR-ALREADY-EXECUTED)
    (try! (stx-transfer? (get amount beq) tx-sender (get beneficiary beq)))
    (map-set bequests { will-id: will-id, index: bequest-index } (merge beq { distributed: true }))
    (ok (get amount beq))))

(define-public (update-executor (will-id uint) (new-executor principal))
  (let ((will (unwrap! (map-get? wills will-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get testator will)) ERR-NOT-AUTHORIZED)
    (map-set wills will-id (merge will { executor: new-executor })) (ok true)))

(define-public (revoke-will (will-id uint))
  (let ((will (unwrap! (map-get? wills will-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get testator will)) ERR-NOT-AUTHORIZED)
    (map-set wills will-id (merge will { status: "revoked" })) (ok true)))

(define-read-only (get-will (id uint)) (map-get? wills id))
(define-read-only (get-bequest (will-id uint) (index uint)) (map-get? bequests { will-id: will-id, index: index }))
(define-read-only (get-will-count) (ok (var-get will-count)))
(define-read-only (get-bequest-total (will-id uint)) (ok (default-to u0 (map-get? bequest-count will-id))))
(define-read-only (get-my-will (who principal)) (map-get? testator-will who))
