;; Ijarah Lease Contract (Islamic Leasing)
;; Asset leasing without interest
;; Halal - ijarah (lease not loan)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-LEASED (err u405))
(define-constant ERR-NOT-LESSEE (err u406))

(define-data-var lease-count uint u0)
(define-data-var total-rent-collected uint u0)

(define-map leases uint {
  lessor: principal, lessee: (optional principal), asset-name: (string-utf8 100),
  monthly-rent: uint, duration-months: uint, payments-made: uint,
  start-block: uint, status: (string-ascii 20)
})
(define-map lease-payments { lease-id: uint, index: uint } { amount: uint, block: uint, payer: principal })

(define-public (create-lease (asset-name (string-utf8 100)) (monthly-rent uint) (duration uint))
  (let ((id (+ (var-get lease-count) u1)))
    (map-set leases id {
      lessor: tx-sender, lessee: none, asset-name: asset-name,
      monthly-rent: monthly-rent, duration-months: duration, payments-made: u0,
      start-block: u0, status: "available"
    })
    (var-set lease-count id) (ok id)))

(define-public (sign-lease (lease-id uint))
  (let ((lease (unwrap! (map-get? leases lease-id) ERR-NOT-FOUND)))
    (asserts! (is-eq (get status lease) "available") ERR-ALREADY-LEASED)
    (map-set leases lease-id (merge lease { lessee: (some tx-sender), start-block: stacks-block-height, status: "active" }))
    (ok true)))

(define-public (pay-rent (lease-id uint))
  (let ((lease (unwrap! (map-get? leases lease-id) ERR-NOT-FOUND)))
    (asserts! (match (get lessee lease) l (is-eq tx-sender l) false) ERR-NOT-LESSEE)
    (asserts! (is-eq (get status lease) "active") ERR-NOT-FOUND)
    (try! (stx-transfer? (get monthly-rent lease) tx-sender (get lessor lease)))
    (let ((new-payments (+ (get payments-made lease) u1)))
      (map-set lease-payments { lease-id: lease-id, index: (get payments-made lease) } { amount: (get monthly-rent lease), block: stacks-block-height, payer: tx-sender })
      (map-set leases lease-id (merge lease {
        payments-made: new-payments,
        status: (if (>= new-payments (get duration-months lease)) "completed" "active")
      }))
      (var-set total-rent-collected (+ (var-get total-rent-collected) (get monthly-rent lease)))
      (ok new-payments))))

(define-public (terminate-lease (lease-id uint))
  (let ((lease (unwrap! (map-get? leases lease-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get lessor lease)) ERR-NOT-AUTHORIZED)
    (map-set leases lease-id (merge lease { status: "terminated" })) (ok true)))

(define-read-only (get-lease (id uint)) (map-get? leases id))
(define-read-only (get-payment (lease-id uint) (index uint)) (map-get? lease-payments { lease-id: lease-id, index: index }))
(define-read-only (get-lease-count) (ok (var-get lease-count)))
(define-read-only (get-total-rent) (ok (var-get total-rent-collected)))
(define-read-only (get-remaining-payments (id uint))
  (match (map-get? leases id) l (ok (- (get duration-months l) (get payments-made l))) (ok u0)))
