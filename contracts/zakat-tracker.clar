;; Zakat Tracker Contract
;; Calculate and track zakat (obligatory charity in Islam)
;; Halal - fulfills Islamic obligation
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant NISAB u2625000000) ;; Nisab threshold in micro-STX (~2625 STX)
(define-constant ZAKAT-RATE u25) ;; 2.5% = 25/1000

(define-data-var total-zakat-paid uint u0)
(define-data-var payer-count uint u0)

(define-map zakat-records principal { total-wealth: uint, zakat-due: uint, zakat-paid: uint, last-calculated: uint })
(define-map zakat-payments { payer: principal, index: uint } { amount: uint, recipient: principal, paid-at: uint })
(define-map payment-count principal uint)

(define-public (calculate-zakat (total-wealth uint))
  (let ((zakat-due (if (>= total-wealth NISAB) (/ (* total-wealth ZAKAT-RATE) u1000) u0)))
    (map-set zakat-records tx-sender { total-wealth: total-wealth, zakat-due: zakat-due, zakat-paid: (default-to u0 (get zakat-paid (map-get? zakat-records tx-sender))), last-calculated: stacks-block-height })
    (ok zakat-due)))

(define-public (pay-zakat (recipient principal) (amount uint))
  (let (
    (record (default-to { total-wealth: u0, zakat-due: u0, zakat-paid: u0, last-calculated: u0 } (map-get? zakat-records tx-sender)))
    (idx (default-to u0 (map-get? payment-count tx-sender)))
  )
    (try! (stx-transfer? amount tx-sender recipient))
    (map-set zakat-payments { payer: tx-sender, index: idx } { amount: amount, recipient: recipient, paid-at: stacks-block-height })
    (map-set payment-count tx-sender (+ idx u1))
    (map-set zakat-records tx-sender (merge record { zakat-paid: (+ (get zakat-paid record) amount) }))
    (var-set total-zakat-paid (+ (var-get total-zakat-paid) amount))
    (ok amount)))

(define-read-only (get-zakat-record (who principal)) (map-get? zakat-records who))
(define-read-only (get-zakat-payment (payer principal) (index uint)) (map-get? zakat-payments { payer: payer, index: index }))
(define-read-only (get-total-zakat-paid) (ok (var-get total-zakat-paid)))
(define-read-only (get-nisab) (ok NISAB))
(define-read-only (get-zakat-rate) (ok ZAKAT-RATE))
(define-read-only (get-remaining-zakat (who principal))
  (match (map-get? zakat-records who)
    r (ok (if (> (get zakat-due r) (get zakat-paid r)) (- (get zakat-due r) (get zakat-paid r)) u0))
    (ok u0)))
