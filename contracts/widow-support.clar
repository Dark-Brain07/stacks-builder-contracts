;; Widow Support Contract
;; Widow financial support program
;; Halal - caring for widows (ihsan)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var beneficiary-count uint u0)
(define-data-var total-support uint u0)
(define-data-var pool-balance uint u0)

(define-map beneficiaries uint { name: (string-utf8 100), dependents: uint, monthly-need: uint, total-received: uint, active: bool, enrolled: uint })
(define-map monthly-payments { beneficiary-id: uint, month: uint } { amount: uint, block: uint })
(define-map donor-records principal { total-donated: uint, donations-count: uint })

(define-public (register-beneficiary (name (string-utf8 100)) (dependents uint) (monthly-need uint))
  (let ((id (+ (var-get beneficiary-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set beneficiaries id { name: name, dependents: dependents, monthly-need: monthly-need, total-received: u0, active: true, enrolled: stacks-block-height })
    (var-set beneficiary-count id) (ok id)))

(define-public (donate-support (amount uint))
  (let ((prev (default-to { total-donated: u0, donations-count: u0 } (map-get? donor-records tx-sender))))
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set donor-records tx-sender { total-donated: (+ (get total-donated prev) amount), donations-count: (+ (get donations-count prev) u1) })
    (var-set pool-balance (+ (var-get pool-balance) amount))
    (var-set total-support (+ (var-get total-support) amount)) (ok amount)))

(define-public (disburse-monthly (beneficiary-id uint) (month uint))
  (let ((b (unwrap! (map-get? beneficiaries beneficiary-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (get active b) ERR-NOT-FOUND)
    (asserts! (>= (var-get pool-balance) (get monthly-need b)) ERR-NOT-FOUND)
    (try! (stx-transfer? (get monthly-need b) CONTRACT-OWNER tx-sender))
    (map-set monthly-payments { beneficiary-id: beneficiary-id, month: month } { amount: (get monthly-need b), block: stacks-block-height })
    (map-set beneficiaries beneficiary-id (merge b { total-received: (+ (get total-received b) (get monthly-need b)) }))
    (var-set pool-balance (- (var-get pool-balance) (get monthly-need b))) (ok (get monthly-need b))))

(define-public (deactivate (beneficiary-id uint))
  (let ((b (unwrap! (map-get? beneficiaries beneficiary-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set beneficiaries beneficiary-id (merge b { active: false })) (ok true)))

(define-read-only (get-beneficiary (id uint)) (map-get? beneficiaries id))
(define-read-only (get-payment (id uint) (month uint)) (map-get? monthly-payments { beneficiary-id: id, month: month }))
(define-read-only (get-donor (who principal)) (map-get? donor-records who))
(define-read-only (get-beneficiary-count) (ok (var-get beneficiary-count)))
(define-read-only (get-pool-balance) (ok (var-get pool-balance)))
