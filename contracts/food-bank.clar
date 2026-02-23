;; Food Bank Contract
;; Food bank inventory and distribution
;; Halal - feeding the hungry
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-OUT-OF-STOCK (err u405))

(define-data-var item-count uint u0)
(define-data-var total-distributed uint u0)
(define-data-var volunteer-count uint u0)

(define-map food-items uint { name: (string-utf8 100), category: (string-ascii 20), quantity: uint, expiry-block: uint, donor: principal, added: uint })
(define-map distribution-log uint { item-id: uint, recipient: principal, quantity: uint, volunteer: principal, block: uint })
(define-data-var dist-count uint u0)
(define-map volunteers principal { name: (string-utf8 50), distributions: uint, active: bool })
(define-map donor-stats principal uint)

(define-public (add-volunteer (vol principal) (name (string-utf8 50)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set volunteers vol { name: name, distributions: u0, active: true })
    (var-set volunteer-count (+ (var-get volunteer-count) u1)) (ok true)))

(define-public (donate-food (name (string-utf8 100)) (category (string-ascii 20)) (quantity uint) (shelf-life uint))
  (let ((id (+ (var-get item-count) u1)))
    (map-set food-items id { name: name, category: category, quantity: quantity, expiry-block: (+ stacks-block-height shelf-life), donor: tx-sender, added: stacks-block-height })
    (map-set donor-stats tx-sender (+ (default-to u0 (map-get? donor-stats tx-sender)) u1))
    (var-set item-count id) (ok id)))

(define-public (distribute (item-id uint) (recipient principal) (quantity uint))
  (let (
    (item (unwrap! (map-get? food-items item-id) ERR-NOT-FOUND))
    (vol (unwrap! (map-get? volunteers tx-sender) ERR-NOT-AUTHORIZED))
    (did (+ (var-get dist-count) u1))
  )
    (asserts! (get active vol) ERR-NOT-AUTHORIZED)
    (asserts! (>= (get quantity item) quantity) ERR-OUT-OF-STOCK)
    (map-set food-items item-id (merge item { quantity: (- (get quantity item) quantity) }))
    (map-set distribution-log did { item-id: item-id, recipient: recipient, quantity: quantity, volunteer: tx-sender, block: stacks-block-height })
    (map-set volunteers tx-sender (merge vol { distributions: (+ (get distributions vol) u1) }))
    (var-set dist-count did)
    (var-set total-distributed (+ (var-get total-distributed) quantity))
    (ok did)))

(define-public (restock (item-id uint) (quantity uint))
  (let ((item (unwrap! (map-get? food-items item-id) ERR-NOT-FOUND)))
    (map-set food-items item-id (merge item { quantity: (+ (get quantity item) quantity) })) (ok true)))

(define-read-only (get-item (id uint)) (map-get? food-items id))
(define-read-only (get-distribution (id uint)) (map-get? distribution-log id))
(define-read-only (get-volunteer (who principal)) (map-get? volunteers who))
(define-read-only (get-item-count) (ok (var-get item-count)))
(define-read-only (get-total-distributed) (ok (var-get total-distributed)))
