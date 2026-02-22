;; Asset Registry Contract
;; Register and track digital/physical assets on-chain
;; Halal - asset documentation
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var asset-count uint u0)

(define-map assets uint { owner: principal, name: (string-utf8 100), category: (string-ascii 20), value: uint, registered: uint, active: bool })
(define-map asset-owner-count principal uint)

(define-public (register-asset (name (string-utf8 100)) (category (string-ascii 20)) (value uint))
  (let ((id (+ (var-get asset-count) u1)))
    (map-set assets id { owner: tx-sender, name: name, category: category, value: value, registered: stacks-block-height, active: true })
    (map-set asset-owner-count tx-sender (+ (default-to u0 (map-get? asset-owner-count tx-sender)) u1))
    (var-set asset-count id) (ok id)))

(define-public (transfer-asset (id uint) (new-owner principal))
  (let ((asset (unwrap! (map-get? assets id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner asset)) ERR-NOT-AUTHORIZED)
    (map-set assets id (merge asset { owner: new-owner }))
    (map-set asset-owner-count tx-sender (- (default-to u1 (map-get? asset-owner-count tx-sender)) u1))
    (map-set asset-owner-count new-owner (+ (default-to u0 (map-get? asset-owner-count new-owner)) u1))
    (ok true)))

(define-public (update-value (id uint) (new-value uint))
  (let ((asset (unwrap! (map-get? assets id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner asset)) ERR-NOT-AUTHORIZED)
    (map-set assets id (merge asset { value: new-value })) (ok true)))

(define-public (deactivate-asset (id uint))
  (let ((asset (unwrap! (map-get? assets id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner asset)) ERR-NOT-AUTHORIZED)
    (map-set assets id (merge asset { active: false })) (ok true)))

(define-read-only (get-asset (id uint)) (map-get? assets id))
(define-read-only (get-asset-count) (ok (var-get asset-count)))
(define-read-only (get-owner-asset-count (owner principal)) (ok (default-to u0 (map-get? asset-owner-count owner))))
