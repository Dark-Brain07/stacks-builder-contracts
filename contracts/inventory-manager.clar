;; Inventory Manager Contract
;; Track business inventory on-chain
;; Halal - business operations
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT (err u405))

(define-data-var item-count uint u0)

(define-map items uint { name: (string-utf8 100), category: (string-ascii 20), quantity: uint, unit-price: uint, added: uint, active: bool })
(define-map managers principal bool)
(define-map item-history { item-id: uint, index: uint } { action: (string-ascii 10), quantity: uint, by: principal, block: uint })
(define-map item-history-count uint uint)

(map-set managers CONTRACT-OWNER true)

(define-public (add-manager (mgr principal))
  (begin (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED) (map-set managers mgr true) (ok true)))

(define-public (add-item (name (string-utf8 100)) (category (string-ascii 20)) (quantity uint) (price uint))
  (let ((id (+ (var-get item-count) u1)))
    (asserts! (default-to false (map-get? managers tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set items id { name: name, category: category, quantity: quantity, unit-price: price, added: stacks-block-height, active: true })
    (var-set item-count id) (ok id)))

(define-public (restock (item-id uint) (quantity uint))
  (let (
    (item (unwrap! (map-get? items item-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? item-history-count item-id)))
  )
    (asserts! (default-to false (map-get? managers tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set items item-id (merge item { quantity: (+ (get quantity item) quantity) }))
    (map-set item-history { item-id: item-id, index: idx } { action: "restock", quantity: quantity, by: tx-sender, block: stacks-block-height })
    (map-set item-history-count item-id (+ idx u1)) (ok true)))

(define-public (sell-item (item-id uint) (quantity uint))
  (let (
    (item (unwrap! (map-get? items item-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? item-history-count item-id)))
  )
    (asserts! (default-to false (map-get? managers tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (>= (get quantity item) quantity) ERR-INSUFFICIENT)
    (map-set items item-id (merge item { quantity: (- (get quantity item) quantity) }))
    (map-set item-history { item-id: item-id, index: idx } { action: "sold", quantity: quantity, by: tx-sender, block: stacks-block-height })
    (map-set item-history-count item-id (+ idx u1)) (ok true)))

(define-public (update-price (item-id uint) (new-price uint))
  (let ((item (unwrap! (map-get? items item-id) ERR-NOT-FOUND)))
    (asserts! (default-to false (map-get? managers tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set items item-id (merge item { unit-price: new-price })) (ok true)))

(define-read-only (get-item (id uint)) (map-get? items id))
(define-read-only (get-item-count) (ok (var-get item-count)))
(define-read-only (get-item-history (item-id uint) (index uint)) (map-get? item-history { item-id: item-id, index: index }))
(define-read-only (get-inventory-value (id uint))
  (match (map-get? items id) i (ok (* (get quantity i) (get unit-price i))) (ok u0)))
