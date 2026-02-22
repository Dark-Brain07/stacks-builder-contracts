;; Waqf Fund Contract (Islamic Endowment)
;; Perpetual charitable endowment - principal preserved, profits distributed
;; Halal - fulfills waqf Islamic principle
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT (err u405))

(define-data-var total-endowment uint u0)
(define-data-var total-yield-distributed uint u0)
(define-data-var beneficiary-count uint u0)

(define-map endowments principal { amount: uint, contributed-at: uint })
(define-map beneficiaries principal { category: (string-utf8 50), share-pct: uint, total-received: uint, active: bool })
(define-map yield-distributions uint { amount: uint, distributed-at: uint })
(define-data-var distribution-count uint u0)

(define-public (contribute-endowment (amount uint))
  (let ((prev (default-to { amount: u0, contributed-at: u0 } (map-get? endowments tx-sender))))
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set endowments tx-sender { amount: (+ (get amount prev) amount), contributed-at: stacks-block-height })
    (var-set total-endowment (+ (var-get total-endowment) amount))
    (print { event: "waqf-contribution", donor: tx-sender, amount: amount })
    (ok amount)))

(define-public (add-beneficiary (who principal) (category (string-utf8 50)) (share-pct uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set beneficiaries who { category: category, share-pct: share-pct, total-received: u0, active: true })
    (var-set beneficiary-count (+ (var-get beneficiary-count) u1))
    (ok true)))

(define-public (distribute-yield (beneficiary principal) (amount uint))
  (let (
    (ben (unwrap! (map-get? beneficiaries beneficiary) ERR-NOT-FOUND))
    (id (+ (var-get distribution-count) u1))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (get active ben) ERR-NOT-FOUND)
    (try! (stx-transfer? amount tx-sender beneficiary))
    (map-set beneficiaries beneficiary (merge ben { total-received: (+ (get total-received ben) amount) }))
    (map-set yield-distributions id { amount: amount, distributed-at: stacks-block-height })
    (var-set distribution-count id)
    (var-set total-yield-distributed (+ (var-get total-yield-distributed) amount))
    (ok amount)))

(define-public (remove-beneficiary (who principal))
  (let ((ben (unwrap! (map-get? beneficiaries who) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set beneficiaries who (merge ben { active: false }))
    (var-set beneficiary-count (- (var-get beneficiary-count) u1))
    (ok true)))

(define-read-only (get-endowment (who principal)) (map-get? endowments who))
(define-read-only (get-beneficiary (who principal)) (map-get? beneficiaries who))
(define-read-only (get-total-endowment) (ok (var-get total-endowment)))
(define-read-only (get-total-distributed) (ok (var-get total-yield-distributed)))
(define-read-only (get-beneficiary-count) (ok (var-get beneficiary-count)))
