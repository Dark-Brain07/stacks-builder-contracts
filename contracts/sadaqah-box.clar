;; Sadaqah Box Contract
;; Digital sadaqah (voluntary charity) collection
;; Halal - sadaqah (voluntary giving)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var box-count uint u0)
(define-data-var total-collected uint u0)
(define-data-var total-given uint u0)

(define-map sadaqah-boxes uint {
  name: (string-utf8 100), custodian: principal, purpose: (string-utf8 200),
  collected: uint, given-out: uint, contributors: uint,
  active: bool, created: uint
})
(define-map contributions { box-id: uint, contributor: principal } { total: uint, count: uint })
(define-map distributions { box-id: uint, index: uint } { recipient: principal, amount: uint, reason: (string-utf8 100), block: uint })
(define-map dist-count uint uint)

(define-public (create-box (name (string-utf8 100)) (purpose (string-utf8 200)))
  (let ((id (+ (var-get box-count) u1)))
    (map-set sadaqah-boxes id { name: name, custodian: tx-sender, purpose: purpose, collected: u0, given-out: u0, contributors: u0, active: true, created: stacks-block-height })
    (var-set box-count id) (ok id)))

(define-public (give-sadaqah (box-id uint) (amount uint))
  (let (
    (box (unwrap! (map-get? sadaqah-boxes box-id) ERR-NOT-FOUND))
    (prev (default-to { total: u0, count: u0 } (map-get? contributions { box-id: box-id, contributor: tx-sender })))
    (is-new (is-eq (get total prev) u0))
  )
    (asserts! (get active box) ERR-NOT-FOUND)
    (try! (stx-transfer? amount tx-sender (get custodian box)))
    (map-set contributions { box-id: box-id, contributor: tx-sender } { total: (+ (get total prev) amount), count: (+ (get count prev) u1) })
    (map-set sadaqah-boxes box-id (merge box { collected: (+ (get collected box) amount), contributors: (if is-new (+ (get contributors box) u1) (get contributors box)) }))
    (var-set total-collected (+ (var-get total-collected) amount))
    (print { event: "sadaqah", box: box-id, giver: tx-sender, amount: amount })
    (ok amount)))

(define-public (distribute-sadaqah (box-id uint) (recipient principal) (amount uint) (reason (string-utf8 100)))
  (let (
    (box (unwrap! (map-get? sadaqah-boxes box-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? dist-count box-id)))
  )
    (asserts! (is-eq tx-sender (get custodian box)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? amount tx-sender recipient))
    (map-set distributions { box-id: box-id, index: idx } { recipient: recipient, amount: amount, reason: reason, block: stacks-block-height })
    (map-set dist-count box-id (+ idx u1))
    (map-set sadaqah-boxes box-id (merge box { given-out: (+ (get given-out box) amount) }))
    (var-set total-given (+ (var-get total-given) amount))
    (ok amount)))

(define-public (close-box (box-id uint))
  (let ((box (unwrap! (map-get? sadaqah-boxes box-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get custodian box)) ERR-NOT-AUTHORIZED)
    (map-set sadaqah-boxes box-id (merge box { active: false })) (ok true)))

(define-read-only (get-box (id uint)) (map-get? sadaqah-boxes id))
(define-read-only (get-contribution (box-id uint) (who principal)) (map-get? contributions { box-id: box-id, contributor: who }))
(define-read-only (get-distribution (box-id uint) (index uint)) (map-get? distributions { box-id: box-id, index: index }))
(define-read-only (get-box-count) (ok (var-get box-count)))
(define-read-only (get-total-collected) (ok (var-get total-collected)))
