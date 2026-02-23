;; Donation Receipt Contract
;; Issue verifiable donation receipts on-chain
;; Halal - charity accountability
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-NOT-CHARITY (err u405))

(define-data-var receipt-count uint u0)
(define-data-var total-donated uint u0)

(define-map charities principal { name: (string-utf8 100), verified: bool, total-received: uint, receipts-issued: uint })
(define-map receipts uint { donor: principal, charity: principal, amount: uint, purpose: (string-utf8 100), tax-deductible: bool, issued: uint })
(define-map donor-totals principal uint)

(define-public (register-charity (name (string-utf8 100)))
  (begin (map-set charities tx-sender { name: name, verified: false, total-received: u0, receipts-issued: u0 }) (ok true)))

(define-public (verify-charity (charity principal))
  (let ((c (unwrap! (map-get? charities charity) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set charities charity (merge c { verified: true })) (ok true)))

(define-public (donate-and-receipt (charity principal) (amount uint) (purpose (string-utf8 100)))
  (let (
    (c (unwrap! (map-get? charities charity) ERR-NOT-CHARITY))
    (id (+ (var-get receipt-count) u1))
  )
    (asserts! (get verified c) ERR-NOT-CHARITY)
    (try! (stx-transfer? amount tx-sender charity))
    (map-set receipts id { donor: tx-sender, charity: charity, amount: amount, purpose: purpose, tax-deductible: true, issued: stacks-block-height })
    (map-set charities charity (merge c { total-received: (+ (get total-received c) amount), receipts-issued: (+ (get receipts-issued c) u1) }))
    (map-set donor-totals tx-sender (+ (default-to u0 (map-get? donor-totals tx-sender)) amount))
    (var-set receipt-count id)
    (var-set total-donated (+ (var-get total-donated) amount))
    (print { event: "donation-receipt", id: id, donor: tx-sender, amount: amount })
    (ok id)))

(define-read-only (get-receipt (id uint)) (map-get? receipts id))
(define-read-only (get-charity (who principal)) (map-get? charities who))
(define-read-only (get-donor-total (who principal)) (ok (default-to u0 (map-get? donor-totals who))))
(define-read-only (get-receipt-count) (ok (var-get receipt-count)))
(define-read-only (get-total-donated) (ok (var-get total-donated)))
