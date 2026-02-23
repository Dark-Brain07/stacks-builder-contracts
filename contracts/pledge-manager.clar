;; Pledge Manager Contract
;; Track public pledges and commitments
;; Halal - accountability and trust
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-FULFILLED (err u405))

(define-data-var pledge-count uint u0)
(define-data-var total-pledged uint u0)
(define-data-var total-fulfilled uint u0)

(define-map pledges uint {
  pledger: principal, description: (string-utf8 200), amount: uint,
  deadline: uint, fulfilled: bool, witness: (optional principal), created: uint
})

(define-public (make-pledge (description (string-utf8 200)) (amount uint) (deadline uint) (witness (optional principal)))
  (let ((id (+ (var-get pledge-count) u1)))
    (map-set pledges id {
      pledger: tx-sender, description: description, amount: amount,
      deadline: (+ stacks-block-height deadline), fulfilled: false, witness: witness, created: stacks-block-height
    })
    (var-set pledge-count id)
    (var-set total-pledged (+ (var-get total-pledged) amount))
    (ok id)))

(define-public (fulfill-pledge (pledge-id uint) (recipient principal))
  (let ((pledge (unwrap! (map-get? pledges pledge-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get pledger pledge)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get fulfilled pledge)) ERR-ALREADY-FULFILLED)
    (try! (stx-transfer? (get amount pledge) tx-sender recipient))
    (map-set pledges pledge-id (merge pledge { fulfilled: true }))
    (var-set total-fulfilled (+ (var-get total-fulfilled) (get amount pledge)))
    (print { event: "pledge-fulfilled", id: pledge-id })
    (ok true)))

(define-public (verify-fulfillment (pledge-id uint))
  (let ((pledge (unwrap! (map-get? pledges pledge-id) ERR-NOT-FOUND)))
    (asserts! (match (get witness pledge) w (is-eq tx-sender w) (is-eq tx-sender CONTRACT-OWNER)) ERR-NOT-AUTHORIZED)
    (map-set pledges pledge-id (merge pledge { fulfilled: true }))
    (var-set total-fulfilled (+ (var-get total-fulfilled) (get amount pledge)))
    (ok true)))

(define-read-only (get-pledge (id uint)) (map-get? pledges id))
(define-read-only (get-pledge-count) (ok (var-get pledge-count)))
(define-read-only (get-total-pledged) (ok (var-get total-pledged)))
(define-read-only (get-total-fulfilled) (ok (var-get total-fulfilled)))
(define-read-only (is-overdue (id uint))
  (match (map-get? pledges id) p (ok (and (not (get fulfilled p)) (> stacks-block-height (get deadline p)))) (ok false)))
