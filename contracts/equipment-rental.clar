;; Equipment Rental Contract
;; Equipment and tool rental marketplace
;; Halal - legitimate asset rental
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-UNAVAILABLE (err u405))

(define-data-var equipment-count uint u0)
(define-data-var rental-count uint u0)
(define-data-var total-revenue uint u0)

(define-map equipment uint { owner: principal, name: (string-utf8 100), category: (string-ascii 20), daily-rate: uint, deposit: uint, available: bool, times-rented: uint })
(define-map rentals uint { equipment-id: uint, renter: principal, start: uint, duration: uint, total-cost: uint, deposit-paid: uint, returned: bool, block: uint })

(define-public (list-equipment (name (string-utf8 100)) (category (string-ascii 20)) (daily-rate uint) (deposit uint))
  (let ((id (+ (var-get equipment-count) u1)))
    (map-set equipment id { owner: tx-sender, name: name, category: category, daily-rate: daily-rate, deposit: deposit, available: true, times-rented: u0 })
    (var-set equipment-count id) (ok id)))

(define-public (rent-equipment (equip-id uint) (duration uint))
  (let (
    (equip (unwrap! (map-get? equipment equip-id) ERR-NOT-FOUND))
    (cost (+ (* (get daily-rate equip) duration) (get deposit equip)))
    (rid (+ (var-get rental-count) u1))
  )
    (asserts! (get available equip) ERR-UNAVAILABLE)
    (try! (stx-transfer? cost tx-sender (get owner equip)))
    (map-set rentals rid { equipment-id: equip-id, renter: tx-sender, start: stacks-block-height, duration: duration, total-cost: (* (get daily-rate equip) duration), deposit-paid: (get deposit equip), returned: false, block: stacks-block-height })
    (map-set equipment equip-id (merge equip { available: false, times-rented: (+ (get times-rented equip) u1) }))
    (var-set rental-count rid)
    (var-set total-revenue (+ (var-get total-revenue) (* (get daily-rate equip) duration)))
    (ok rid)))

(define-public (return-equipment (rental-id uint))
  (let (
    (rental (unwrap! (map-get? rentals rental-id) ERR-NOT-FOUND))
    (equip (unwrap! (map-get? equipment (get equipment-id rental)) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get owner equip)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? (get deposit-paid rental) tx-sender (get renter rental)))
    (map-set rentals rental-id (merge rental { returned: true }))
    (map-set equipment (get equipment-id rental) (merge equip { available: true }))
    (ok true)))

(define-public (update-rate (equip-id uint) (new-rate uint))
  (let ((equip (unwrap! (map-get? equipment equip-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner equip)) ERR-NOT-AUTHORIZED)
    (map-set equipment equip-id (merge equip { daily-rate: new-rate })) (ok true)))

(define-read-only (get-equipment (id uint)) (map-get? equipment id))
(define-read-only (get-rental (id uint)) (map-get? rentals id))
(define-read-only (get-equipment-count) (ok (var-get equipment-count)))
(define-read-only (get-rental-count) (ok (var-get rental-count)))
(define-read-only (get-total-revenue) (ok (var-get total-revenue)))
