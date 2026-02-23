;; Taxi Fare Contract
;; Ride-hailing fare management
;; Halal - honest fare pricing
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant PLATFORM-FEE-PCT u5)

(define-data-var trip-count uint u0)
(define-data-var total-fares uint u0)

(define-map drivers principal { name: (string-utf8 100), vehicle: (string-utf8 50), trips: uint, earned: uint, rating-total: uint, ratings: uint, active: bool })
(define-map trips uint {
  driver: principal, rider: principal, origin: (string-utf8 100), destination: (string-utf8 100),
  fare: uint, distance-km: uint, status: (string-ascii 20), started: uint
})

(define-public (register-driver (name (string-utf8 100)) (vehicle (string-utf8 50)))
  (begin (map-set drivers tx-sender { name: name, vehicle: vehicle, trips: u0, earned: u0, rating-total: u0, ratings: u0, active: true }) (ok true)))

(define-public (request-ride (driver principal) (origin (string-utf8 100)) (destination (string-utf8 100)) (fare uint))
  (let (
    (d (unwrap! (map-get? drivers driver) ERR-NOT-FOUND))
    (platform-fee (/ (* fare PLATFORM-FEE-PCT) u100))
    (driver-pay (- fare platform-fee))
    (tid (+ (var-get trip-count) u1))
  )
    (asserts! (get active d) ERR-NOT-FOUND)
    (try! (stx-transfer? driver-pay tx-sender driver))
    (try! (stx-transfer? platform-fee tx-sender CONTRACT-OWNER))
    (map-set trips tid { driver: driver, rider: tx-sender, origin: origin, destination: destination, fare: fare, distance-km: u0, status: "in-progress", started: stacks-block-height })
    (var-set trip-count tid) (ok tid)))

(define-public (complete-trip (trip-id uint) (distance uint))
  (let (
    (t (unwrap! (map-get? trips trip-id) ERR-NOT-FOUND))
    (d (unwrap! (map-get? drivers (get driver t)) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get driver t)) ERR-NOT-AUTHORIZED)
    (map-set trips trip-id (merge t { distance-km: distance, status: "completed" }))
    (map-set drivers tx-sender (merge d { trips: (+ (get trips d) u1), earned: (+ (get earned d) (get fare t)) }))
    (var-set total-fares (+ (var-get total-fares) (get fare t))) (ok true)))

(define-public (rate-driver (trip-id uint) (rating uint))
  (let (
    (t (unwrap! (map-get? trips trip-id) ERR-NOT-FOUND))
    (d (unwrap! (map-get? drivers (get driver t)) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get rider t)) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-NOT-AUTHORIZED)
    (map-set drivers (get driver t) (merge d { rating-total: (+ (get rating-total d) rating), ratings: (+ (get ratings d) u1) }))
    (ok rating)))

(define-public (toggle-driver (active bool))
  (let ((d (unwrap! (map-get? drivers tx-sender) ERR-NOT-FOUND)))
    (map-set drivers tx-sender (merge d { active: active })) (ok true)))

(define-read-only (get-driver (who principal)) (map-get? drivers who))
(define-read-only (get-trip (id uint)) (map-get? trips id))
(define-read-only (get-trip-count) (ok (var-get trip-count)))
(define-read-only (get-total-fares) (ok (var-get total-fares)))
