;; Rent Payment Contract
;; On-chain rent payment tracking
;; Halal - fair housing
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-PAID (err u405))
(define-constant ERR-LEASE-INACTIVE (err u406))

(define-data-var lease-count uint u0)
(define-data-var total-collected uint u0)

(define-map leases uint {
  landlord: principal, tenant: principal, monthly-rent: uint,
  start-block: uint, end-block: uint, payments-made: uint, active: bool
})
(define-map rent-payments { lease-id: uint, period: uint } { amount: uint, paid-at: uint })

(define-public (create-lease (tenant principal) (monthly-rent uint) (duration-months uint))
  (let ((id (+ (var-get lease-count) u1)))
    (map-set leases id {
      landlord: tx-sender, tenant: tenant, monthly-rent: monthly-rent,
      start-block: stacks-block-height, end-block: (+ stacks-block-height (* duration-months u4320)),
      payments-made: u0, active: true
    })
    (var-set lease-count id) (ok id)))

(define-public (pay-rent (lease-id uint))
  (let (
    (lease (unwrap! (map-get? leases lease-id) ERR-NOT-FOUND))
    (period (get payments-made lease))
  )
    (asserts! (is-eq tx-sender (get tenant lease)) ERR-NOT-AUTHORIZED)
    (asserts! (get active lease) ERR-LEASE-INACTIVE)
    (asserts! (is-none (map-get? rent-payments { lease-id: lease-id, period: period })) ERR-ALREADY-PAID)
    (try! (stx-transfer? (get monthly-rent lease) tx-sender (get landlord lease)))
    (map-set rent-payments { lease-id: lease-id, period: period } { amount: (get monthly-rent lease), paid-at: stacks-block-height })
    (map-set leases lease-id (merge lease { payments-made: (+ period u1) }))
    (var-set total-collected (+ (var-get total-collected) (get monthly-rent lease)))
    (ok (+ period u1))))

(define-public (terminate-lease (lease-id uint))
  (let ((lease (unwrap! (map-get? leases lease-id) ERR-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender (get landlord lease)) (is-eq tx-sender (get tenant lease))) ERR-NOT-AUTHORIZED)
    (map-set leases lease-id (merge lease { active: false })) (ok true)))

(define-read-only (get-lease (id uint)) (map-get? leases id))
(define-read-only (get-payment (lease-id uint) (period uint)) (map-get? rent-payments { lease-id: lease-id, period: period }))
(define-read-only (get-lease-count) (ok (var-get lease-count)))
(define-read-only (get-total-collected) (ok (var-get total-collected)))
