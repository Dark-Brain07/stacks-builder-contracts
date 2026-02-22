;; Parking Share Contract
;; Parking space sharing marketplace
;; Halal - resource sharing
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-UNAVAILABLE (err u405))

(define-data-var spot-count uint u0)
(define-data-var booking-count uint u0)
(define-data-var total-revenue uint u0)

(define-map parking-spots uint { owner: principal, location: (string-utf8 100), rate-per-block: uint, available: bool, bookings-total: uint })
(define-map bookings uint { spot-id: uint, driver: principal, start-block: uint, duration: uint, paid: uint, status: (string-ascii 20) })

(define-public (list-spot (location (string-utf8 100)) (rate uint))
  (let ((id (+ (var-get spot-count) u1)))
    (map-set parking-spots id { owner: tx-sender, location: location, rate-per-block: rate, available: true, bookings-total: u0 })
    (var-set spot-count id) (ok id)))

(define-public (book-spot (spot-id uint) (duration uint))
  (let (
    (spot (unwrap! (map-get? parking-spots spot-id) ERR-NOT-FOUND))
    (cost (* duration (get rate-per-block spot)))
    (bid (+ (var-get booking-count) u1))
  )
    (asserts! (get available spot) ERR-UNAVAILABLE)
    (try! (stx-transfer? cost tx-sender (get owner spot)))
    (map-set bookings bid { spot-id: spot-id, driver: tx-sender, start-block: stacks-block-height, duration: duration, paid: cost, status: "active" })
    (map-set parking-spots spot-id (merge spot { available: false, bookings-total: (+ (get bookings-total spot) u1) }))
    (var-set booking-count bid)
    (var-set total-revenue (+ (var-get total-revenue) cost)) (ok bid)))

(define-public (end-booking (booking-id uint))
  (let (
    (b (unwrap! (map-get? bookings booking-id) ERR-NOT-FOUND))
    (spot (unwrap! (map-get? parking-spots (get spot-id b)) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get driver b)) ERR-NOT-AUTHORIZED)
    (map-set bookings booking-id (merge b { status: "ended" }))
    (map-set parking-spots (get spot-id b) (merge spot { available: true })) (ok true)))

(define-public (toggle-availability (spot-id uint) (available bool))
  (let ((spot (unwrap! (map-get? parking-spots spot-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner spot)) ERR-NOT-AUTHORIZED)
    (map-set parking-spots spot-id (merge spot { available: available })) (ok true)))

(define-read-only (get-spot (id uint)) (map-get? parking-spots id))
(define-read-only (get-booking (id uint)) (map-get? bookings id))
(define-read-only (get-spot-count) (ok (var-get spot-count)))
(define-read-only (get-booking-count) (ok (var-get booking-count)))
(define-read-only (get-total-revenue) (ok (var-get total-revenue)))
