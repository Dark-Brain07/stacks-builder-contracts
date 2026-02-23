;; Sadaqah Jariyah Contract (Ongoing Charity)
;; Continuous charity that keeps giving rewards
;; Halal - fulfills sadaqah jariyah principle
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var cause-count uint u0)
(define-data-var total-donated uint u0)

(define-map causes uint {
  name: (string-utf8 100), category: (string-utf8 50), guardian: principal,
  total-raised: uint, disbursed: uint, donors: uint, active: bool, created: uint
})
(define-map donor-records { cause-id: uint, donor: principal } { total: uint, last-donation: uint, frequency: uint })
(define-map disbursements { cause-id: uint, index: uint } { amount: uint, recipient: principal, purpose: (string-utf8 100), block: uint })
(define-map disbursement-count uint uint)

(define-public (create-cause (name (string-utf8 100)) (category (string-utf8 50)) (guardian principal))
  (let ((id (+ (var-get cause-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set causes id { name: name, category: category, guardian: guardian, total-raised: u0, disbursed: u0, donors: u0, active: true, created: stacks-block-height })
    (var-set cause-count id) (ok id)))

(define-public (donate (cause-id uint) (amount uint))
  (let (
    (cause (unwrap! (map-get? causes cause-id) ERR-NOT-FOUND))
    (prev (default-to { total: u0, last-donation: u0, frequency: u0 } (map-get? donor-records { cause-id: cause-id, donor: tx-sender })))
    (is-new (is-eq (get total prev) u0))
  )
    (asserts! (get active cause) ERR-NOT-FOUND)
    (try! (stx-transfer? amount tx-sender (get guardian cause)))
    (map-set donor-records { cause-id: cause-id, donor: tx-sender } { total: (+ (get total prev) amount), last-donation: stacks-block-height, frequency: (+ (get frequency prev) u1) })
    (map-set causes cause-id (merge cause { total-raised: (+ (get total-raised cause) amount), donors: (if is-new (+ (get donors cause) u1) (get donors cause)) }))
    (var-set total-donated (+ (var-get total-donated) amount))
    (print { event: "sadaqah-jariyah", donor: tx-sender, cause: cause-id, amount: amount })
    (ok amount)))

(define-public (disburse (cause-id uint) (recipient principal) (amount uint) (purpose (string-utf8 100)))
  (let (
    (cause (unwrap! (map-get? causes cause-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? disbursement-count cause-id)))
  )
    (asserts! (is-eq tx-sender (get guardian cause)) ERR-NOT-AUTHORIZED)
    (map-set disbursements { cause-id: cause-id, index: idx } { amount: amount, recipient: recipient, purpose: purpose, block: stacks-block-height })
    (map-set disbursement-count cause-id (+ idx u1))
    (map-set causes cause-id (merge cause { disbursed: (+ (get disbursed cause) amount) }))
    (ok true)))

(define-read-only (get-cause (id uint)) (map-get? causes id))
(define-read-only (get-donor (cause-id uint) (donor principal)) (map-get? donor-records { cause-id: cause-id, donor: donor }))
(define-read-only (get-disbursement (cause-id uint) (index uint)) (map-get? disbursements { cause-id: cause-id, index: index }))
(define-read-only (get-cause-count) (ok (var-get cause-count)))
(define-read-only (get-total-donated) (ok (var-get total-donated)))
