;; Parking Meter Contract
;; Smart parking meter management
;; Halal - fair usage pricing
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-OCCUPIED (err u405))

(define-data-var meter-count uint u0)
(define-data-var total-collected uint u0)

(define-map meters uint { zone: (string-ascii 20), location: (string-utf8 100), rate-per-block: uint, occupied: bool, current-user: (optional principal), expires: uint, revenue: uint })
(define-map parking-sessions { meter-id: uint, index: uint } { user: principal, paid: uint, start: uint, end: uint })
(define-map session-count uint uint)

(define-public (install-meter (zone (string-ascii 20)) (location (string-utf8 100)) (rate uint))
  (let ((id (+ (var-get meter-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set meters id { zone: zone, location: location, rate-per-block: rate, occupied: false, current-user: none, expires: u0, revenue: u0 })
    (var-set meter-count id) (ok id)))

(define-public (park (meter-id uint) (duration uint))
  (let (
    (meter (unwrap! (map-get? meters meter-id) ERR-NOT-FOUND))
    (cost (* duration (get rate-per-block meter)))
    (idx (default-to u0 (map-get? session-count meter-id)))
  )
    (asserts! (not (get occupied meter)) ERR-OCCUPIED)
    (try! (stx-transfer? cost tx-sender CONTRACT-OWNER))
    (map-set meters meter-id (merge meter { occupied: true, current-user: (some tx-sender), expires: (+ stacks-block-height duration), revenue: (+ (get revenue meter) cost) }))
    (map-set parking-sessions { meter-id: meter-id, index: idx } { user: tx-sender, paid: cost, start: stacks-block-height, end: (+ stacks-block-height duration) })
    (map-set session-count meter-id (+ idx u1))
    (var-set total-collected (+ (var-get total-collected) cost)) (ok cost)))

(define-public (extend-parking (meter-id uint) (extra-blocks uint))
  (let (
    (meter (unwrap! (map-get? meters meter-id) ERR-NOT-FOUND))
    (cost (* extra-blocks (get rate-per-block meter)))
  )
    (asserts! (is-eq (some tx-sender) (get current-user meter)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? cost tx-sender CONTRACT-OWNER))
    (map-set meters meter-id (merge meter { expires: (+ (get expires meter) extra-blocks), revenue: (+ (get revenue meter) cost) }))
    (var-set total-collected (+ (var-get total-collected) cost)) (ok cost)))

(define-public (vacate (meter-id uint))
  (let ((meter (unwrap! (map-get? meters meter-id) ERR-NOT-FOUND)))
    (asserts! (is-eq (some tx-sender) (get current-user meter)) ERR-NOT-AUTHORIZED)
    (map-set meters meter-id (merge meter { occupied: false, current-user: none })) (ok true)))

(define-public (update-rate (meter-id uint) (new-rate uint))
  (let ((meter (unwrap! (map-get? meters meter-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set meters meter-id (merge meter { rate-per-block: new-rate })) (ok true)))

(define-read-only (get-meter (id uint)) (map-get? meters id))
(define-read-only (get-session (meter-id uint) (index uint)) (map-get? parking-sessions { meter-id: meter-id, index: index }))
(define-read-only (get-meter-count) (ok (var-get meter-count)))
(define-read-only (get-total-collected) (ok (var-get total-collected)))
