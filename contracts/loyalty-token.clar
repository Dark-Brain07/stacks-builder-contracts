;; Loyalty Token Contract
;; Fungible loyalty points for merchants
;; Halal - customer reward program
;; Clarity 4 compatible

(define-fungible-token loyalty-point)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT (err u405))
(define-constant ERR-NOT-MERCHANT (err u406))

(define-data-var registered-merchants uint u0)
(define-data-var total-points-issued uint u0)
(define-data-var total-points-redeemed uint u0)

(define-map merchants principal { name: (string-utf8 100), points-issued: uint, active: bool })
(define-map redemption-offers uint { merchant: principal, description: (string-utf8 100), points-cost: uint, available: uint, redeemed: uint })
(define-data-var offer-count uint u0)

(define-public (register-merchant (name (string-utf8 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set merchants tx-sender { name: name, points-issued: u0, active: true })
    (var-set registered-merchants (+ (var-get registered-merchants) u1)) (ok true)))

(define-public (add-merchant (merchant principal) (name (string-utf8 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set merchants merchant { name: name, points-issued: u0, active: true })
    (var-set registered-merchants (+ (var-get registered-merchants) u1)) (ok true)))

(define-public (award-points (customer principal) (amount uint))
  (let ((m (unwrap! (map-get? merchants tx-sender) ERR-NOT-MERCHANT)))
    (asserts! (get active m) ERR-NOT-MERCHANT)
    (try! (ft-mint? loyalty-point amount customer))
    (map-set merchants tx-sender (merge m { points-issued: (+ (get points-issued m) amount) }))
    (var-set total-points-issued (+ (var-get total-points-issued) amount))
    (ok amount)))

(define-public (create-offer (description (string-utf8 100)) (points-cost uint) (available uint))
  (let ((id (+ (var-get offer-count) u1)))
    (asserts! (is-some (map-get? merchants tx-sender)) ERR-NOT-MERCHANT)
    (map-set redemption-offers id { merchant: tx-sender, description: description, points-cost: points-cost, available: available, redeemed: u0 })
    (var-set offer-count id) (ok id)))

(define-public (redeem-offer (offer-id uint))
  (let ((offer (unwrap! (map-get? redemption-offers offer-id) ERR-NOT-FOUND)))
    (asserts! (>= (ft-get-balance loyalty-point tx-sender) (get points-cost offer)) ERR-INSUFFICIENT)
    (asserts! (< (get redeemed offer) (get available offer)) ERR-NOT-FOUND)
    (try! (ft-burn? loyalty-point (get points-cost offer) tx-sender))
    (map-set redemption-offers offer-id (merge offer { redeemed: (+ (get redeemed offer) u1) }))
    (var-set total-points-redeemed (+ (var-get total-points-redeemed) (get points-cost offer)))
    (ok true)))

(define-public (transfer-points (amount uint) (to principal))
  (ft-transfer? loyalty-point amount tx-sender to))

(define-read-only (get-balance (who principal)) (ok (ft-get-balance loyalty-point who)))
(define-read-only (get-offer (id uint)) (map-get? redemption-offers id))
(define-read-only (get-merchant (who principal)) (map-get? merchants who))
(define-read-only (get-total-issued) (ok (var-get total-points-issued)))
(define-read-only (get-total-redeemed) (ok (var-get total-points-redeemed)))
