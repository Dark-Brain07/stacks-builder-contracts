;; Crop Takaful Contract (Agricultural Insurance)
;; Islamic cooperative crop insurance
;; Halal - agricultural protection
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-INSURED (err u405))

(define-data-var policy-count uint u0)
(define-data-var pool-balance uint u0)
(define-data-var total-claims uint u0)

(define-map policies uint {
  farmer: principal, crop-type: (string-utf8 50), coverage: uint,
  premium: uint, season-end: uint, claimed: bool, active: bool
})
(define-map farmer-policies principal uint)

(define-public (buy-policy (crop-type (string-utf8 50)) (coverage uint) (season-length uint))
  (let (
    (id (+ (var-get policy-count) u1))
    (premium (/ (* coverage u5) u100))
  )
    (asserts! (is-none (map-get? farmer-policies tx-sender)) ERR-ALREADY-INSURED)
    (try! (stx-transfer? premium tx-sender CONTRACT-OWNER))
    (map-set policies id {
      farmer: tx-sender, crop-type: crop-type, coverage: coverage,
      premium: premium, season-end: (+ stacks-block-height season-length), claimed: false, active: true
    })
    (map-set farmer-policies tx-sender id)
    (var-set pool-balance (+ (var-get pool-balance) premium))
    (var-set policy-count id) (ok id)))

(define-public (file-claim (policy-id uint) (damage-pct uint))
  (let (
    (policy (unwrap! (map-get? policies policy-id) ERR-NOT-FOUND))
    (payout (/ (* (get coverage policy) damage-pct) u100))
  )
    (asserts! (is-eq tx-sender (get farmer policy)) ERR-NOT-AUTHORIZED)
    (asserts! (get active policy) ERR-NOT-FOUND)
    (asserts! (not (get claimed policy)) ERR-NOT-FOUND)
    (print { event: "claim-filed", policy: policy-id, farmer: tx-sender, damage: damage-pct, payout: payout })
    (ok payout)))

(define-public (approve-payout (policy-id uint) (amount uint))
  (let ((policy (unwrap! (map-get? policies policy-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? amount tx-sender (get farmer policy)))
    (map-set policies policy-id (merge policy { claimed: true }))
    (var-set pool-balance (- (var-get pool-balance) amount))
    (var-set total-claims (+ (var-get total-claims) amount))
    (ok amount)))

(define-public (end-season (policy-id uint))
  (let ((policy (unwrap! (map-get? policies policy-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set policies policy-id (merge policy { active: false }))
    (map-delete farmer-policies (get farmer policy))
    (ok true)))

(define-read-only (get-policy (id uint)) (map-get? policies id))
(define-read-only (get-farmer-policy (farmer principal)) (map-get? farmer-policies farmer))
(define-read-only (get-pool-balance) (ok (var-get pool-balance)))
(define-read-only (get-policy-count) (ok (var-get policy-count)))
(define-read-only (get-total-claims) (ok (var-get total-claims)))
