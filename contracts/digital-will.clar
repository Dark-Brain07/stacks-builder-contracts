;; Digital Will Contract
;; Digital asset will and legacy planning
;; Halal - responsible estate planning
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-NOT-EXECUTOR (err u405))

(define-data-var will-count uint u0)

(define-map wills uint {
  testator: principal, executor: principal,
  instructions: (string-utf8 200), total-value: uint,
  beneficiary-count: uint, executed: bool, created: uint
})
(define-map will-beneficiaries { will-id: uint, index: uint } {
  beneficiary: principal, share-pct: uint, amount: uint, distributed: bool
})
(define-map testator-will principal uint)
(define-map witnesses { will-id: uint, witness: principal } { attested: uint })
(define-map witness-count uint uint)

(define-public (create-will (executor principal) (instructions (string-utf8 200)) (total-value uint))
  (let ((id (+ (var-get will-count) u1)))
    (try! (stx-transfer? total-value tx-sender CONTRACT-OWNER))
    (map-set wills id { testator: tx-sender, executor: executor, instructions: instructions, total-value: total-value, beneficiary-count: u0, executed: false, created: stacks-block-height })
    (map-set testator-will tx-sender id)
    (var-set will-count id) (ok id)))

(define-public (add-beneficiary (will-id uint) (beneficiary principal) (share-pct uint))
  (let (
    (will (unwrap! (map-get? wills will-id) ERR-NOT-FOUND))
    (idx (get beneficiary-count will))
    (amount (/ (* (get total-value will) share-pct) u100))
  )
    (asserts! (is-eq tx-sender (get testator will)) ERR-NOT-AUTHORIZED)
    (map-set will-beneficiaries { will-id: will-id, index: idx } { beneficiary: beneficiary, share-pct: share-pct, amount: amount, distributed: false })
    (map-set wills will-id (merge will { beneficiary-count: (+ idx u1) }))
    (ok idx)))

(define-public (witness-will (will-id uint))
  (let ((wc (default-to u0 (map-get? witness-count will-id))))
    (asserts! (is-some (map-get? wills will-id)) ERR-NOT-FOUND)
    (map-set witnesses { will-id: will-id, witness: tx-sender } { attested: stacks-block-height })
    (map-set witness-count will-id (+ wc u1))
    (ok true)))

(define-public (execute-will (will-id uint) (beneficiary-index uint))
  (let (
    (will (unwrap! (map-get? wills will-id) ERR-NOT-FOUND))
    (ben (unwrap! (map-get? will-beneficiaries { will-id: will-id, index: beneficiary-index }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get executor will)) ERR-NOT-EXECUTOR)
    (asserts! (not (get distributed ben)) ERR-NOT-FOUND)
    (asserts! (>= (default-to u0 (map-get? witness-count will-id)) u2) ERR-NOT-FOUND)
    (try! (stx-transfer? (get amount ben) CONTRACT-OWNER (get beneficiary ben)))
    (map-set will-beneficiaries { will-id: will-id, index: beneficiary-index } (merge ben { distributed: true }))
    (ok (get amount ben))))

(define-read-only (get-will (id uint)) (map-get? wills id))
(define-read-only (get-beneficiary (will-id uint) (index uint)) (map-get? will-beneficiaries { will-id: will-id, index: index }))
(define-read-only (get-will-by-testator (who principal)) (map-get? testator-will who))
(define-read-only (get-will-count) (ok (var-get will-count)))
(define-read-only (get-witness-count (will-id uint)) (ok (default-to u0 (map-get? witness-count will-id))))
