;; Delivery Tracker Contract
;; Track order deliveries on-chain
;; Halal - transparent logistics
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-DELIVERED (err u405))

(define-data-var order-count uint u0)
(define-data-var delivery-count uint u0)

(define-map orders uint {
  sender: principal, receiver: principal, courier: (optional principal),
  description: (string-utf8 100), status: (string-ascii 20), created: uint, delivered-at: uint
})
(define-map order-updates { order-id: uint, index: uint } { status: (string-ascii 20), location: (string-utf8 100), updated-by: principal, block: uint })
(define-map update-count uint uint)
(define-map couriers principal { deliveries: uint, active: bool })

(define-public (create-order (receiver principal) (description (string-utf8 100)))
  (let ((id (+ (var-get order-count) u1)))
    (map-set orders id { sender: tx-sender, receiver: receiver, courier: none, description: description, status: "pending", created: stacks-block-height, delivered-at: u0 })
    (var-set order-count id) (ok id)))

(define-public (assign-courier (order-id uint) (courier principal))
  (let ((order (unwrap! (map-get? orders order-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get sender order)) ERR-NOT-AUTHORIZED)
    (map-set orders order-id (merge order { courier: (some courier), status: "assigned" }))
    (ok true)))

(define-public (update-delivery (order-id uint) (status (string-ascii 20)) (location (string-utf8 100)))
  (let (
    (order (unwrap! (map-get? orders order-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? update-count order-id)))
  )
    (asserts! (match (get courier order) c (is-eq tx-sender c) (is-eq tx-sender (get sender order))) ERR-NOT-AUTHORIZED)
    (map-set order-updates { order-id: order-id, index: idx } { status: status, location: location, updated-by: tx-sender, block: stacks-block-height })
    (map-set update-count order-id (+ idx u1))
    (map-set orders order-id (merge order { status: status }))
    (ok idx)))

(define-public (confirm-delivery (order-id uint))
  (let ((order (unwrap! (map-get? orders order-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get receiver order)) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-eq (get status order) "delivered")) ERR-ALREADY-DELIVERED)
    (map-set orders order-id (merge order { status: "delivered", delivered-at: stacks-block-height }))
    (match (get courier order) c
      (map-set couriers c { deliveries: (+ (get deliveries (default-to { deliveries: u0, active: true } (map-get? couriers c))) u1), active: true })
      true)
    (var-set delivery-count (+ (var-get delivery-count) u1))
    (ok true)))

(define-read-only (get-order (id uint)) (map-get? orders id))
(define-read-only (get-update (order-id uint) (index uint)) (map-get? order-updates { order-id: order-id, index: index }))
(define-read-only (get-order-count) (ok (var-get order-count)))
(define-read-only (get-delivery-count) (ok (var-get delivery-count)))
(define-read-only (get-courier (who principal)) (map-get? couriers who))
