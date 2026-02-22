;; Emergency Fund Contract
;; Community emergency relief fund
;; Halal - mutual aid
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-CLAIMED (err u405))

(define-data-var fund-balance uint u0)
(define-data-var request-count uint u0)
(define-data-var total-disbursed uint u0)

(define-map contributors principal { total: uint, contributions: uint })
(define-map relief-requests uint {
  requester: principal, reason: (string-utf8 200), amount: uint,
  urgency: uint, status: (string-ascii 20), created: uint
})
(define-map approvers principal bool)

(map-set approvers CONTRACT-OWNER true)

(define-public (add-approver (a principal))
  (begin (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED) (map-set approvers a true) (ok true)))

(define-public (contribute (amount uint))
  (let ((prev (default-to { total: u0, contributions: u0 } (map-get? contributors tx-sender))))
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set contributors tx-sender { total: (+ (get total prev) amount), contributions: (+ (get contributions prev) u1) })
    (var-set fund-balance (+ (var-get fund-balance) amount))
    (ok amount)))

(define-public (request-relief (reason (string-utf8 200)) (amount uint) (urgency uint))
  (let ((id (+ (var-get request-count) u1)))
    (map-set relief-requests id { requester: tx-sender, reason: reason, amount: amount, urgency: urgency, status: "pending", created: stacks-block-height })
    (var-set request-count id) (ok id)))

(define-public (approve-relief (request-id uint))
  (let ((req (unwrap! (map-get? relief-requests request-id) ERR-NOT-FOUND)))
    (asserts! (default-to false (map-get? approvers tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status req) "pending") ERR-ALREADY-CLAIMED)
    (try! (stx-transfer? (get amount req) CONTRACT-OWNER (get requester req)))
    (map-set relief-requests request-id (merge req { status: "approved" }))
    (var-set fund-balance (- (var-get fund-balance) (get amount req)))
    (var-set total-disbursed (+ (var-get total-disbursed) (get amount req)))
    (ok (get amount req))))

(define-public (deny-relief (request-id uint))
  (let ((req (unwrap! (map-get? relief-requests request-id) ERR-NOT-FOUND)))
    (asserts! (default-to false (map-get? approvers tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set relief-requests request-id (merge req { status: "denied" })) (ok true)))

(define-read-only (get-request (id uint)) (map-get? relief-requests id))
(define-read-only (get-contributor (who principal)) (map-get? contributors who))
(define-read-only (get-fund-balance) (ok (var-get fund-balance)))
(define-read-only (get-request-count) (ok (var-get request-count)))
(define-read-only (get-total-disbursed) (ok (var-get total-disbursed)))
