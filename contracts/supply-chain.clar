;; Supply Chain Tracking Contract
;; Track product journey from source to consumer
;; Halal - transparency and trust
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-PRODUCT-NOT-FOUND (err u404))
(define-constant ERR-NOT-HANDLER (err u403))

(define-data-var product-count uint u0)
(define-data-var checkpoint-count uint u0)

(define-map products uint { name: (string-utf8 100), origin: (string-utf8 100), creator: principal, status: (string-ascii 20), created: uint })
(define-map handlers principal bool)
(define-map checkpoints { product-id: uint, index: uint } { handler: principal, location: (string-utf8 100), status: (string-ascii 20), timestamp: uint })
(define-map product-checkpoint-count uint uint)

(map-set handlers CONTRACT-OWNER true)

(define-public (add-handler (handler principal))
  (begin (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set handlers handler true) (ok true)))

(define-public (create-product (name (string-utf8 100)) (origin (string-utf8 100)))
  (let ((id (+ (var-get product-count) u1)))
    (asserts! (default-to false (map-get? handlers tx-sender)) ERR-NOT-HANDLER)
    (map-set products id { name: name, origin: origin, creator: tx-sender, status: "created", created: stacks-block-height })
    (map-set product-checkpoint-count id u0)
    (var-set product-count id) (ok id)))

(define-public (add-checkpoint (product-id uint) (location (string-utf8 100)) (status (string-ascii 20)))
  (let (
    (product (unwrap! (map-get? products product-id) ERR-PRODUCT-NOT-FOUND))
    (idx (default-to u0 (map-get? product-checkpoint-count product-id)))
  )
    (asserts! (default-to false (map-get? handlers tx-sender)) ERR-NOT-HANDLER)
    (map-set checkpoints { product-id: product-id, index: idx } { handler: tx-sender, location: location, status: status, timestamp: stacks-block-height })
    (map-set product-checkpoint-count product-id (+ idx u1))
    (map-set products product-id (merge product { status: status }))
    (ok idx)))

(define-read-only (get-product (id uint)) (map-get? products id))
(define-read-only (get-checkpoint (product-id uint) (index uint)) (map-get? checkpoints { product-id: product-id, index: index }))
(define-read-only (get-product-count) (ok (var-get product-count)))
(define-read-only (get-checkpoint-total (product-id uint)) (ok (default-to u0 (map-get? product-checkpoint-count product-id))))
