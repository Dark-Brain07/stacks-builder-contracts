;; Qard Hasan Contract (Interest-Free Loan)
;; Islamic benevolent loan - zero interest
;; Halal - no riba, pure goodwill lending
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-REPAID (err u405))
(define-constant ERR-ACTIVE-LOAN (err u406))

(define-data-var loan-count uint u0)
(define-data-var total-lent uint u0)
(define-data-var total-repaid uint u0)

(define-map loans uint {
  lender: principal, borrower: principal, amount: uint,
  repaid: uint, status: (string-ascii 20), created: uint, due-block: uint
})
(define-map active-loans principal uint)

(define-public (create-loan (borrower principal) (amount uint) (duration uint))
  (let ((id (+ (var-get loan-count) u1)))
    (asserts! (is-none (map-get? active-loans borrower)) ERR-ACTIVE-LOAN)
    (try! (stx-transfer? amount tx-sender borrower))
    (map-set loans id {
      lender: tx-sender, borrower: borrower, amount: amount,
      repaid: u0, status: "active", created: stacks-block-height, due-block: (+ stacks-block-height duration)
    })
    (map-set active-loans borrower id)
    (var-set loan-count id)
    (var-set total-lent (+ (var-get total-lent) amount))
    (print { event: "qard-hasan-issued", id: id, borrower: borrower, amount: amount })
    (ok id)))

(define-public (repay (loan-id uint) (amount uint))
  (let (
    (loan (unwrap! (map-get? loans loan-id) ERR-NOT-FOUND))
    (new-repaid (+ (get repaid loan) amount))
  )
    (asserts! (is-eq tx-sender (get borrower loan)) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-eq (get status loan) "repaid")) ERR-ALREADY-REPAID)
    (try! (stx-transfer? amount tx-sender (get lender loan)))
    (if (>= new-repaid (get amount loan))
      (begin
        (map-set loans loan-id (merge loan { repaid: new-repaid, status: "repaid" }))
        (map-delete active-loans tx-sender))
      (map-set loans loan-id (merge loan { repaid: new-repaid })))
    (var-set total-repaid (+ (var-get total-repaid) amount))
    (ok new-repaid)))

(define-public (forgive-loan (loan-id uint))
  (let ((loan (unwrap! (map-get? loans loan-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get lender loan)) ERR-NOT-AUTHORIZED)
    (map-set loans loan-id (merge loan { status: "forgiven" }))
    (map-delete active-loans (get borrower loan))
    (print { event: "loan-forgiven", id: loan-id })
    (ok true)))

(define-read-only (get-loan (id uint)) (map-get? loans id))
(define-read-only (get-active-loan (borrower principal)) (map-get? active-loans borrower))
(define-read-only (get-loan-count) (ok (var-get loan-count)))
(define-read-only (get-total-lent) (ok (var-get total-lent)))
(define-read-only (get-total-repaid) (ok (var-get total-repaid)))
