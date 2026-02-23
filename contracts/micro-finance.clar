;; Micro Finance Contract
;; Halal microfinance for small businesses
;; Zero interest - fee-based model
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ACTIVE-LOAN (err u405))
(define-constant SERVICE-FEE-PCT u2) ;; 2% one-time service fee

(define-data-var loan-count uint u0)
(define-data-var total-disbursed uint u0)

(define-map borrower-profiles principal { business: (string-utf8 100), approved: bool, active-loan: bool, total-borrowed: uint })
(define-map micro-loans uint {
  borrower: principal, principal-amount: uint, service-fee: uint,
  total-due: uint, repaid: uint, status: (string-ascii 20), disbursed: uint
})

(define-public (register-borrower (business (string-utf8 100)))
  (begin
    (map-set borrower-profiles tx-sender { business: business, approved: false, active-loan: false, total-borrowed: u0 })
    (ok true)))

(define-public (approve-borrower (borrower principal))
  (let ((profile (unwrap! (map-get? borrower-profiles borrower) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set borrower-profiles borrower (merge profile { approved: true })) (ok true)))

(define-public (apply-loan (amount uint))
  (let (
    (profile (unwrap! (map-get? borrower-profiles tx-sender) ERR-NOT-FOUND))
    (id (+ (var-get loan-count) u1))
    (fee (/ (* amount SERVICE-FEE-PCT) u100))
  )
    (asserts! (get approved profile) ERR-NOT-AUTHORIZED)
    (asserts! (not (get active-loan profile)) ERR-ACTIVE-LOAN)
    (map-set micro-loans id {
      borrower: tx-sender, principal-amount: amount, service-fee: fee,
      total-due: (+ amount fee), repaid: u0, status: "pending", disbursed: u0
    })
    (var-set loan-count id) (ok id)))

(define-public (disburse-loan (loan-id uint))
  (let ((loan (unwrap! (map-get? micro-loans loan-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? (get principal-amount loan) tx-sender (get borrower loan)))
    (map-set micro-loans loan-id (merge loan { status: "active", disbursed: stacks-block-height }))
    (map-set borrower-profiles (get borrower loan) (merge (default-to { business: u"", approved: true, active-loan: false, total-borrowed: u0 } (map-get? borrower-profiles (get borrower loan))) { active-loan: true, total-borrowed: (+ (get principal-amount loan) (default-to u0 (get total-borrowed (map-get? borrower-profiles (get borrower loan))))) }))
    (var-set total-disbursed (+ (var-get total-disbursed) (get principal-amount loan)))
    (ok true)))

(define-public (make-repayment (loan-id uint) (amount uint))
  (let ((loan (unwrap! (map-get? micro-loans loan-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get borrower loan)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (let ((new-repaid (+ (get repaid loan) amount)))
      (if (>= new-repaid (get total-due loan))
        (begin
          (map-set micro-loans loan-id (merge loan { repaid: new-repaid, status: "repaid" }))
          (map-set borrower-profiles tx-sender (merge (default-to { business: u"", approved: true, active-loan: false, total-borrowed: u0 } (map-get? borrower-profiles tx-sender)) { active-loan: false })))
        (map-set micro-loans loan-id (merge loan { repaid: new-repaid })))
      (ok new-repaid))))

(define-read-only (get-loan (id uint)) (map-get? micro-loans id))
(define-read-only (get-profile (who principal)) (map-get? borrower-profiles who))
(define-read-only (get-loan-count) (ok (var-get loan-count)))
(define-read-only (get-total-disbursed) (ok (var-get total-disbursed)))
