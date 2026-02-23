;; Invoice Manager Contract
;; On-chain invoice creation and payment tracking
;; Halal - legitimate business
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-PAID (err u405))
(define-constant ERR-WRONG-AMOUNT (err u406))

(define-data-var invoice-count uint u0)

(define-map invoices uint {
  issuer: principal, payer: principal, amount: uint,
  description: (string-utf8 200), due-block: uint,
  paid: bool, paid-at: uint
})
(define-map issuer-invoices principal uint)
(define-map payer-invoices principal uint)

(define-public (create-invoice (payer principal) (amount uint) (description (string-utf8 200)) (due-blocks uint))
  (let ((id (+ (var-get invoice-count) u1)))
    (map-set invoices id {
      issuer: tx-sender, payer: payer, amount: amount,
      description: description, due-block: (+ stacks-block-height due-blocks),
      paid: false, paid-at: u0
    })
    (map-set issuer-invoices tx-sender (+ (default-to u0 (map-get? issuer-invoices tx-sender)) u1))
    (map-set payer-invoices payer (+ (default-to u0 (map-get? payer-invoices payer)) u1))
    (var-set invoice-count id) (ok id)))

(define-public (pay-invoice (invoice-id uint))
  (let ((inv (unwrap! (map-get? invoices invoice-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get payer inv)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get paid inv)) ERR-ALREADY-PAID)
    (try! (stx-transfer? (get amount inv) tx-sender (get issuer inv)))
    (map-set invoices invoice-id (merge inv { paid: true, paid-at: stacks-block-height }))
    (print { event: "invoice-paid", id: invoice-id, amount: (get amount inv) })
    (ok true)))

(define-public (cancel-invoice (invoice-id uint))
  (let ((inv (unwrap! (map-get? invoices invoice-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get issuer inv)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get paid inv)) ERR-ALREADY-PAID)
    (map-set invoices invoice-id (merge inv { paid: true, paid-at: u0 })) (ok true)))

(define-read-only (get-invoice (id uint)) (map-get? invoices id))
(define-read-only (get-invoice-count) (ok (var-get invoice-count)))
(define-read-only (get-issuer-count (issuer principal)) (ok (default-to u0 (map-get? issuer-invoices issuer))))
(define-read-only (get-payer-count (payer principal)) (ok (default-to u0 (map-get? payer-invoices payer))))
(define-read-only (is-overdue (invoice-id uint))
  (match (map-get? invoices invoice-id)
    inv (ok (and (not (get paid inv)) (> stacks-block-height (get due-block inv))))
    (ok false)))
