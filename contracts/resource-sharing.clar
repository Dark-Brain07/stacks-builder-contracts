;; Resource Sharing Contract
;; Community shared resource management
;; Halal - cooperative sharing
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-UNAVAILABLE (err u405))

(define-data-var resource-count uint u0)
(define-data-var booking-count uint u0)

(define-map resources uint {
  owner: principal, name: (string-utf8 100), category: (string-ascii 20),
  rate-per-block: uint, available: bool, total-earnings: uint, bookings: uint
})
(define-map bookings uint {
  resource-id: uint, borrower: principal, start-block: uint, end-block: uint,
  total-cost: uint, status: (string-ascii 20)
})
(define-map active-booking uint uint)

(define-public (list-resource (name (string-utf8 100)) (category (string-ascii 20)) (rate uint))
  (let ((id (+ (var-get resource-count) u1)))
    (map-set resources id { owner: tx-sender, name: name, category: category, rate-per-block: rate, available: true, total-earnings: u0, bookings: u0 })
    (var-set resource-count id) (ok id)))

(define-public (book-resource (resource-id uint) (duration uint))
  (let (
    (res (unwrap! (map-get? resources resource-id) ERR-NOT-FOUND))
    (cost (* (get rate-per-block res) duration))
    (bid (+ (var-get booking-count) u1))
  )
    (asserts! (get available res) ERR-UNAVAILABLE)
    (try! (stx-transfer? cost tx-sender (get owner res)))
    (map-set bookings bid { resource-id: resource-id, borrower: tx-sender, start-block: stacks-block-height, end-block: (+ stacks-block-height duration), total-cost: cost, status: "active" })
    (map-set resources resource-id (merge res { available: false, total-earnings: (+ (get total-earnings res) cost), bookings: (+ (get bookings res) u1) }))
    (map-set active-booking resource-id bid)
    (var-set booking-count bid) (ok bid)))

(define-public (return-resource (booking-id uint))
  (let (
    (booking (unwrap! (map-get? bookings booking-id) ERR-NOT-FOUND))
    (res (unwrap! (map-get? resources (get resource-id booking)) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get borrower booking)) ERR-NOT-AUTHORIZED)
    (map-set bookings booking-id (merge booking { status: "returned" }))
    (map-set resources (get resource-id booking) (merge res { available: true }))
    (map-delete active-booking (get resource-id booking))
    (ok true)))

(define-public (update-rate (resource-id uint) (new-rate uint))
  (let ((res (unwrap! (map-get? resources resource-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner res)) ERR-NOT-AUTHORIZED)
    (map-set resources resource-id (merge res { rate-per-block: new-rate })) (ok true)))

(define-read-only (get-resource (id uint)) (map-get? resources id))
(define-read-only (get-booking (id uint)) (map-get? bookings id))
(define-read-only (get-resource-count) (ok (var-get resource-count)))
(define-read-only (get-booking-count) (ok (var-get booking-count)))
(define-read-only (get-active-booking (resource-id uint)) (map-get? active-booking resource-id))
