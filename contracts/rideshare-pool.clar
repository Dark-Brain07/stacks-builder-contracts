;; Rideshare Pool Contract
;; Carpooling coordination
;; Halal - reducing waste, community sharing
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-FULL (err u405))

(define-data-var ride-count uint u0)
(define-data-var total-shared uint u0)

(define-map rides uint {
  driver: principal, origin: (string-utf8 100), destination: (string-utf8 100),
  seats: uint, seats-taken: uint, cost-per-seat: uint,
  departure-block: uint, status: (string-ascii 20)
})
(define-map passengers { ride-id: uint, passenger: principal } { paid: uint, booked: uint })
(define-map driver-stats principal { rides-offered: uint, passengers-carried: uint, earned: uint })

(define-public (offer-ride (origin (string-utf8 100)) (destination (string-utf8 100)) (seats uint) (cost uint) (departure uint))
  (let (
    (id (+ (var-get ride-count) u1))
    (stats (default-to { rides-offered: u0, passengers-carried: u0, earned: u0 } (map-get? driver-stats tx-sender)))
  )
    (map-set rides id { driver: tx-sender, origin: origin, destination: destination, seats: seats, seats-taken: u0, cost-per-seat: cost, departure-block: (+ stacks-block-height departure), status: "open" })
    (map-set driver-stats tx-sender (merge stats { rides-offered: (+ (get rides-offered stats) u1) }))
    (var-set ride-count id) (ok id)))

(define-public (book-ride (ride-id uint))
  (let (
    (ride (unwrap! (map-get? rides ride-id) ERR-NOT-FOUND))
    (stats (default-to { rides-offered: u0, passengers-carried: u0, earned: u0 } (map-get? driver-stats (get driver ride))))
  )
    (asserts! (is-eq (get status ride) "open") ERR-NOT-FOUND)
    (asserts! (< (get seats-taken ride) (get seats ride)) ERR-FULL)
    (try! (stx-transfer? (get cost-per-seat ride) tx-sender (get driver ride)))
    (map-set passengers { ride-id: ride-id, passenger: tx-sender } { paid: (get cost-per-seat ride), booked: stacks-block-height })
    (map-set rides ride-id (merge ride { seats-taken: (+ (get seats-taken ride) u1) }))
    (map-set driver-stats (get driver ride) (merge stats { passengers-carried: (+ (get passengers-carried stats) u1), earned: (+ (get earned stats) (get cost-per-seat ride)) }))
    (var-set total-shared (+ (var-get total-shared) u1)) (ok true)))

(define-public (complete-ride (ride-id uint))
  (let ((ride (unwrap! (map-get? rides ride-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get driver ride)) ERR-NOT-AUTHORIZED)
    (map-set rides ride-id (merge ride { status: "completed" })) (ok true)))

(define-public (cancel-ride (ride-id uint))
  (let ((ride (unwrap! (map-get? rides ride-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get driver ride)) ERR-NOT-AUTHORIZED)
    (map-set rides ride-id (merge ride { status: "cancelled" })) (ok true)))

(define-read-only (get-ride (id uint)) (map-get? rides id))
(define-read-only (get-passenger (ride-id uint) (who principal)) (map-get? passengers { ride-id: ride-id, passenger: who }))
(define-read-only (get-driver-stats (who principal)) (map-get? driver-stats who))
(define-read-only (get-ride-count) (ok (var-get ride-count)))
(define-read-only (get-total-shared) (ok (var-get total-shared)))
