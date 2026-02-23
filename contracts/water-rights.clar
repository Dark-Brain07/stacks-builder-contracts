;; Water Rights Contract
;; Water allocation and usage rights management
;; Halal - natural resource stewardship
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-EXCEEDED (err u405))

(define-data-var allocation-count uint u0)
(define-data-var total-allocated uint u0)
(define-data-var total-consumed uint u0)

(define-map water-rights uint {
  holder: principal, source: (string-utf8 100), allocated-liters: uint,
  consumed-liters: uint, season: (string-ascii 20), expires: uint, active: bool
})
(define-map usage-logs { allocation-id: uint, index: uint } { liters: uint, purpose: (string-utf8 100), block: uint })
(define-map usage-log-count uint uint)
(define-map holder-total principal uint)

(define-public (grant-water-right (holder principal) (source (string-utf8 100)) (liters uint) (season (string-ascii 20)) (validity uint))
  (let ((id (+ (var-get allocation-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set water-rights id { holder: holder, source: source, allocated-liters: liters, consumed-liters: u0, season: season, expires: (+ stacks-block-height validity), active: true })
    (map-set holder-total holder (+ (default-to u0 (map-get? holder-total holder)) liters))
    (var-set allocation-count id)
    (var-set total-allocated (+ (var-get total-allocated) liters))
    (ok id)))

(define-public (log-usage (allocation-id uint) (liters uint) (purpose (string-utf8 100)))
  (let (
    (right (unwrap! (map-get? water-rights allocation-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? usage-log-count allocation-id)))
  )
    (asserts! (is-eq tx-sender (get holder right)) ERR-NOT-AUTHORIZED)
    (asserts! (get active right) ERR-NOT-FOUND)
    (asserts! (<= (+ (get consumed-liters right) liters) (get allocated-liters right)) ERR-EXCEEDED)
    (map-set usage-logs { allocation-id: allocation-id, index: idx } { liters: liters, purpose: purpose, block: stacks-block-height })
    (map-set usage-log-count allocation-id (+ idx u1))
    (map-set water-rights allocation-id (merge right { consumed-liters: (+ (get consumed-liters right) liters) }))
    (var-set total-consumed (+ (var-get total-consumed) liters))
    (ok liters)))

(define-public (transfer-right (allocation-id uint) (new-holder principal))
  (let ((right (unwrap! (map-get? water-rights allocation-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get holder right)) ERR-NOT-AUTHORIZED)
    (map-set water-rights allocation-id (merge right { holder: new-holder })) (ok true)))

(define-public (revoke-right (allocation-id uint))
  (let ((right (unwrap! (map-get? water-rights allocation-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set water-rights allocation-id (merge right { active: false })) (ok true)))

(define-read-only (get-right (id uint)) (map-get? water-rights id))
(define-read-only (get-usage (allocation-id uint) (index uint)) (map-get? usage-logs { allocation-id: allocation-id, index: index }))
(define-read-only (get-allocation-count) (ok (var-get allocation-count)))
(define-read-only (get-total-consumed) (ok (var-get total-consumed)))
(define-read-only (get-remaining (id uint))
  (match (map-get? water-rights id) r (ok (- (get allocated-liters r) (get consumed-liters r))) (ok u0)))
