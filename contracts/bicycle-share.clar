;; Bicycle Share Contract
;; Community bicycle sharing program
;; Halal - eco-friendly transport
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-UNAVAILABLE (err u405))

(define-data-var bike-count uint u0)
(define-data-var rental-count uint u0)
(define-data-var total-revenue uint u0)

(define-map bikes uint { station: (string-utf8 100), bike-type: (string-ascii 20), rate-per-block: uint, available: bool, total-rentals: uint })
(define-map rentals uint { bike-id: uint, rider: principal, start-block: uint, end-block: uint, paid: uint, status: (string-ascii 20) })
(define-map rider-stats principal { total-rides: uint, total-spent: uint })

(define-public (add-bike (station (string-utf8 100)) (bike-type (string-ascii 20)) (rate uint))
  (let ((id (+ (var-get bike-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set bikes id { station: station, bike-type: bike-type, rate-per-block: rate, available: true, total-rentals: u0 })
    (var-set bike-count id) (ok id)))

(define-public (rent-bike (bike-id uint) (duration uint))
  (let (
    (bike (unwrap! (map-get? bikes bike-id) ERR-NOT-FOUND))
    (cost (* duration (get rate-per-block bike)))
    (rid (+ (var-get rental-count) u1))
    (stats (default-to { total-rides: u0, total-spent: u0 } (map-get? rider-stats tx-sender)))
  )
    (asserts! (get available bike) ERR-UNAVAILABLE)
    (try! (stx-transfer? cost tx-sender CONTRACT-OWNER))
    (map-set rentals rid { bike-id: bike-id, rider: tx-sender, start-block: stacks-block-height, end-block: (+ stacks-block-height duration), paid: cost, status: "active" })
    (map-set bikes bike-id (merge bike { available: false, total-rentals: (+ (get total-rentals bike) u1) }))
    (map-set rider-stats tx-sender { total-rides: (+ (get total-rides stats) u1), total-spent: (+ (get total-spent stats) cost) })
    (var-set rental-count rid)
    (var-set total-revenue (+ (var-get total-revenue) cost)) (ok rid)))

(define-public (return-bike (rental-id uint) (station (string-utf8 100)))
  (let (
    (r (unwrap! (map-get? rentals rental-id) ERR-NOT-FOUND))
    (bike (unwrap! (map-get? bikes (get bike-id r)) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get rider r)) ERR-NOT-AUTHORIZED)
    (map-set rentals rental-id (merge r { end-block: stacks-block-height, status: "returned" }))
    (map-set bikes (get bike-id r) (merge bike { available: true, station: station })) (ok true)))

(define-read-only (get-bike (id uint)) (map-get? bikes id))
(define-read-only (get-rental (id uint)) (map-get? rentals id))
(define-read-only (get-rider (who principal)) (map-get? rider-stats who))
(define-read-only (get-bike-count) (ok (var-get bike-count)))
(define-read-only (get-total-revenue) (ok (var-get total-revenue)))
