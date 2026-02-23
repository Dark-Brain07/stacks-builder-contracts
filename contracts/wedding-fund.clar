;; Wedding Fund Contract
;; Community wedding assistance fund
;; Halal - supporting marriage (nikah)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var application-count uint u0)
(define-data-var total-funded uint u0)
(define-data-var pool-balance uint u0)

(define-map applications uint {
  applicant: principal, couple-names: (string-utf8 200), wedding-date: uint,
  amount-needed: uint, amount-received: uint, supporters: uint,
  approved: bool, status: (string-ascii 20), applied: uint
})
(define-map supporters { app-id: uint, supporter: principal } uint)

(define-public (apply-for-fund (couple-names (string-utf8 200)) (wedding-date uint) (amount uint))
  (let ((id (+ (var-get application-count) u1)))
    (map-set applications id { applicant: tx-sender, couple-names: couple-names, wedding-date: (+ stacks-block-height wedding-date), amount-needed: amount, amount-received: u0, supporters: u0, approved: false, status: "pending", applied: stacks-block-height })
    (var-set application-count id) (ok id)))

(define-public (approve-application (app-id uint))
  (let ((app (unwrap! (map-get? applications app-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set applications app-id (merge app { approved: true, status: "approved" })) (ok true)))

(define-public (contribute (app-id uint) (amount uint))
  (let (
    (app (unwrap! (map-get? applications app-id) ERR-NOT-FOUND))
    (prev (default-to u0 (map-get? supporters { app-id: app-id, supporter: tx-sender })))
    (is-new (is-eq prev u0))
  )
    (asserts! (get approved app) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? amount tx-sender (get applicant app)))
    (map-set supporters { app-id: app-id, supporter: tx-sender } (+ prev amount))
    (map-set applications app-id (merge app { amount-received: (+ (get amount-received app) amount), supporters: (if is-new (+ (get supporters app) u1) (get supporters app)) }))
    (var-set total-funded (+ (var-get total-funded) amount)) (ok amount)))

(define-public (donate-pool (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (var-set pool-balance (+ (var-get pool-balance) amount)) (ok amount)))

(define-public (fund-from-pool (app-id uint) (amount uint))
  (let ((app (unwrap! (map-get? applications app-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (>= (var-get pool-balance) amount) ERR-NOT-FOUND)
    (try! (stx-transfer? amount CONTRACT-OWNER (get applicant app)))
    (map-set applications app-id (merge app { amount-received: (+ (get amount-received app) amount) }))
    (var-set pool-balance (- (var-get pool-balance) amount))
    (var-set total-funded (+ (var-get total-funded) amount)) (ok amount)))

(define-read-only (get-application (id uint)) (map-get? applications id))
(define-read-only (get-supporter (app-id uint) (who principal)) (ok (default-to u0 (map-get? supporters { app-id: app-id, supporter: who }))))
(define-read-only (get-application-count) (ok (var-get application-count)))
(define-read-only (get-total-funded) (ok (var-get total-funded)))
(define-read-only (get-pool-balance) (ok (var-get pool-balance)))
