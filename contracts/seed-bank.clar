;; Seed Bank Contract
;; Community seed library and sharing
;; Halal - agriculture and food security
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-OUT-OF-STOCK (err u405))

(define-data-var seed-count uint u0)
(define-data-var checkout-count uint u0)

(define-map seeds uint { name: (string-utf8 100), variety: (string-utf8 50), quantity: uint, donor: principal, season: (string-ascii 20), added: uint })
(define-map checkouts uint { seed-id: uint, borrower: principal, quantity: uint, returned-quantity: uint, checkout-block: uint, returned: bool })
(define-map gardener-log principal { seeds-borrowed: uint, seeds-returned: uint })

(define-public (donate-seeds (name (string-utf8 100)) (variety (string-utf8 50)) (quantity uint) (season (string-ascii 20)))
  (let ((id (+ (var-get seed-count) u1)))
    (map-set seeds id { name: name, variety: variety, quantity: quantity, donor: tx-sender, season: season, added: stacks-block-height })
    (var-set seed-count id) (ok id)))

(define-public (checkout-seeds (seed-id uint) (quantity uint))
  (let (
    (seed (unwrap! (map-get? seeds seed-id) ERR-NOT-FOUND))
    (cid (+ (var-get checkout-count) u1))
    (stats (default-to { seeds-borrowed: u0, seeds-returned: u0 } (map-get? gardener-log tx-sender)))
  )
    (asserts! (>= (get quantity seed) quantity) ERR-OUT-OF-STOCK)
    (map-set seeds seed-id (merge seed { quantity: (- (get quantity seed) quantity) }))
    (map-set checkouts cid { seed-id: seed-id, borrower: tx-sender, quantity: quantity, returned-quantity: u0, checkout-block: stacks-block-height, returned: false })
    (map-set gardener-log tx-sender (merge stats { seeds-borrowed: (+ (get seeds-borrowed stats) quantity) }))
    (var-set checkout-count cid) (ok cid)))

(define-public (return-seeds (checkout-id uint) (quantity uint))
  (let (
    (co (unwrap! (map-get? checkouts checkout-id) ERR-NOT-FOUND))
    (seed (unwrap! (map-get? seeds (get seed-id co)) ERR-NOT-FOUND))
    (stats (default-to { seeds-borrowed: u0, seeds-returned: u0 } (map-get? gardener-log tx-sender)))
  )
    (asserts! (is-eq tx-sender (get borrower co)) ERR-NOT-AUTHORIZED)
    (map-set seeds (get seed-id co) (merge seed { quantity: (+ (get quantity seed) quantity) }))
    (map-set checkouts checkout-id (merge co { returned-quantity: (+ (get returned-quantity co) quantity), returned: true }))
    (map-set gardener-log tx-sender (merge stats { seeds-returned: (+ (get seeds-returned stats) quantity) }))
    (ok quantity)))

(define-public (restock (seed-id uint) (quantity uint))
  (let ((seed (unwrap! (map-get? seeds seed-id) ERR-NOT-FOUND)))
    (map-set seeds seed-id (merge seed { quantity: (+ (get quantity seed) quantity) })) (ok true)))

(define-read-only (get-seed (id uint)) (map-get? seeds id))
(define-read-only (get-checkout (id uint)) (map-get? checkouts id))
(define-read-only (get-gardener (who principal)) (map-get? gardener-log who))
(define-read-only (get-seed-count) (ok (var-get seed-count)))
(define-read-only (get-checkout-count) (ok (var-get checkout-count)))
