;; Land Registry Contract
;; On-chain property/land registration
;; Halal - asset documentation
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-EXISTS (err u405))

(define-data-var property-count uint u0)

(define-map properties uint {
  owner: principal, title: (string-utf8 100), location: (string-utf8 200),
  area-sqm: uint, registered-at: uint, verified: bool
})
(define-map owner-properties principal (list 20 uint))
(define-map transfer-history { property-id: uint, index: uint } { from: principal, to: principal, block: uint, price: uint })
(define-map transfer-count uint uint)

(define-public (register-property (title (string-utf8 100)) (location (string-utf8 200)) (area uint))
  (let ((id (+ (var-get property-count) u1)))
    (map-set properties id { owner: tx-sender, title: title, location: location, area-sqm: area, registered-at: stacks-block-height, verified: false })
    (var-set property-count id) (ok id)))

(define-public (verify-property (id uint))
  (let ((prop (unwrap! (map-get? properties id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set properties id (merge prop { verified: true })) (ok true)))

(define-public (transfer-property (id uint) (new-owner principal) (price uint))
  (let (
    (prop (unwrap! (map-get? properties id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? transfer-count id)))
  )
    (asserts! (is-eq tx-sender (get owner prop)) ERR-NOT-AUTHORIZED)
    (if (> price u0) (try! (stx-transfer? price new-owner tx-sender)) true)
    (map-set transfer-history { property-id: id, index: idx } { from: tx-sender, to: new-owner, block: stacks-block-height, price: price })
    (map-set transfer-count id (+ idx u1))
    (map-set properties id (merge prop { owner: new-owner }))
    (ok true)))

(define-read-only (get-property (id uint)) (map-get? properties id))
(define-read-only (get-property-count) (ok (var-get property-count)))
(define-read-only (get-transfer (property-id uint) (index uint)) (map-get? transfer-history { property-id: property-id, index: index }))
(define-read-only (get-transfer-total (property-id uint)) (ok (default-to u0 (map-get? transfer-count property-id))))
