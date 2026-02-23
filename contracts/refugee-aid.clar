;; Refugee Aid Contract
;; Refugee assistance and resettlement coordination
;; Halal - helping the displaced (muhajir)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var case-count uint u0)
(define-data-var total-aid uint u0)

(define-map aid-cases uint {
  caseworker: principal, family-size: uint, origin: (string-utf8 100),
  needs: (string-utf8 200), aid-received: uint, status: (string-ascii 20), opened: uint
})
(define-map aid-donors { case-id: uint, donor: principal } uint)
(define-map aid-disbursements { case-id: uint, index: uint } { amount: uint, purpose: (string-utf8 100), block: uint })
(define-map disbursement-count uint uint)

(define-public (open-case (family-size uint) (origin (string-utf8 100)) (needs (string-utf8 200)))
  (let ((id (+ (var-get case-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set aid-cases id { caseworker: tx-sender, family-size: family-size, origin: origin, needs: needs, aid-received: u0, status: "active", opened: stacks-block-height })
    (var-set case-count id) (ok id)))

(define-public (donate-aid (case-id uint) (amount uint))
  (let (
    (c (unwrap! (map-get? aid-cases case-id) ERR-NOT-FOUND))
    (prev (default-to u0 (map-get? aid-donors { case-id: case-id, donor: tx-sender })))
  )
    (try! (stx-transfer? amount tx-sender (get caseworker c)))
    (map-set aid-donors { case-id: case-id, donor: tx-sender } (+ prev amount))
    (map-set aid-cases case-id (merge c { aid-received: (+ (get aid-received c) amount) }))
    (var-set total-aid (+ (var-get total-aid) amount)) (ok amount)))

(define-public (disburse-aid (case-id uint) (amount uint) (purpose (string-utf8 100)))
  (let (
    (c (unwrap! (map-get? aid-cases case-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? disbursement-count case-id)))
  )
    (asserts! (is-eq tx-sender (get caseworker c)) ERR-NOT-AUTHORIZED)
    (map-set aid-disbursements { case-id: case-id, index: idx } { amount: amount, purpose: purpose, block: stacks-block-height })
    (map-set disbursement-count case-id (+ idx u1)) (ok idx)))

(define-public (close-case (case-id uint))
  (let ((c (unwrap! (map-get? aid-cases case-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get caseworker c)) ERR-NOT-AUTHORIZED)
    (map-set aid-cases case-id (merge c { status: "resettled" })) (ok true)))

(define-read-only (get-case (id uint)) (map-get? aid-cases id))
(define-read-only (get-donor (case-id uint) (donor principal)) (ok (default-to u0 (map-get? aid-donors { case-id: case-id, donor: donor }))))
(define-read-only (get-disbursement (case-id uint) (index uint)) (map-get? aid-disbursements { case-id: case-id, index: index }))
(define-read-only (get-case-count) (ok (var-get case-count)))
(define-read-only (get-total-aid) (ok (var-get total-aid)))
