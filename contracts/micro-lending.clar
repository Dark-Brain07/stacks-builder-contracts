;; Micro Lending Contract
;; Qard hasan (interest-free) micro-lending pool
;; Halal - no riba, benevolent loans
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT (err u405))

(define-data-var loan-count uint u0)
(define-data-var pool-balance uint u0)
(define-data-var total-lent uint u0)
(define-data-var total-repaid uint u0)

(define-map loans uint { borrower: principal, amount: uint, repaid: uint, purpose: (string-utf8 200), approved: bool, fully-repaid: bool, created: uint })
(define-map borrower-profiles principal { loans-taken: uint, total-borrowed: uint, total-repaid: uint, on-time: uint })
(define-map pool-contributors principal uint)

(define-public (contribute-pool (amount uint))
  (let ((prev (default-to u0 (map-get? pool-contributors tx-sender))))
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set pool-contributors tx-sender (+ prev amount))
    (var-set pool-balance (+ (var-get pool-balance) amount)) (ok amount)))

(define-public (request-loan (amount uint) (purpose (string-utf8 200)))
  (let ((id (+ (var-get loan-count) u1)))
    (map-set loans id { borrower: tx-sender, amount: amount, repaid: u0, purpose: purpose, approved: false, fully-repaid: false, created: stacks-block-height })
    (var-set loan-count id) (ok id)))

(define-public (approve-loan (loan-id uint))
  (let ((l (unwrap! (map-get? loans loan-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (>= (var-get pool-balance) (get amount l)) ERR-INSUFFICIENT)
    (try! (stx-transfer? (get amount l) CONTRACT-OWNER (get borrower l)))
    (let ((bp (default-to { loans-taken: u0, total-borrowed: u0, total-repaid: u0, on-time: u0 } (map-get? borrower-profiles (get borrower l)))))
      (map-set borrower-profiles (get borrower l) (merge bp { loans-taken: (+ (get loans-taken bp) u1), total-borrowed: (+ (get total-borrowed bp) (get amount l)) })))
    (map-set loans loan-id (merge l { approved: true }))
    (var-set pool-balance (- (var-get pool-balance) (get amount l)))
    (var-set total-lent (+ (var-get total-lent) (get amount l))) (ok (get amount l))))

(define-public (repay (loan-id uint) (amount uint))
  (let (
    (l (unwrap! (map-get? loans loan-id) ERR-NOT-FOUND))
    (bp (default-to { loans-taken: u0, total-borrowed: u0, total-repaid: u0, on-time: u0 } (map-get? borrower-profiles (get borrower l))))
    (new-repaid (+ (get repaid l) amount))
  )
    (asserts! (is-eq tx-sender (get borrower l)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set loans loan-id (merge l { repaid: new-repaid, fully-repaid: (>= new-repaid (get amount l)) }))
    (map-set borrower-profiles tx-sender (merge bp { total-repaid: (+ (get total-repaid bp) amount) }))
    (var-set pool-balance (+ (var-get pool-balance) amount))
    (var-set total-repaid (+ (var-get total-repaid) amount)) (ok new-repaid)))

(define-read-only (get-loan (id uint)) (map-get? loans id))
(define-read-only (get-borrower (who principal)) (map-get? borrower-profiles who))
(define-read-only (get-contributor (who principal)) (ok (default-to u0 (map-get? pool-contributors who))))
(define-read-only (get-pool-balance) (ok (var-get pool-balance)))
(define-read-only (get-loan-count) (ok (var-get loan-count)))
(define-read-only (get-total-lent) (ok (var-get total-lent)))
