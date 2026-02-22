;; Sukuk Bond Contract (Islamic Bond)
;; Asset-backed investment certificates
;; Halal - sukuk (not interest-based bonds)
;; Clarity 4 compatible

(define-fungible-token sukuk-cert)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT (err u405))
(define-constant ERR-MATURED (err u406))

(define-data-var issuance-count uint u0)
(define-data-var total-issued uint u0)
(define-data-var total-profit-paid uint u0)

(define-map sukuk-issuances uint {
  name: (string-utf8 100), underlying-asset: (string-utf8 100),
  face-value: uint, total-certificates: uint, sold: uint,
  profit-rate: uint, maturity-block: uint, status: (string-ascii 20)
})
(define-map holders principal { certificates: uint, invested: uint, profit-claimed: uint })

(define-public (issue-sukuk (name (string-utf8 100)) (asset (string-utf8 100)) (face-value uint) (total uint) (profit-rate uint) (maturity-blocks uint))
  (let ((id (+ (var-get issuance-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set sukuk-issuances id {
      name: name, underlying-asset: asset, face-value: face-value,
      total-certificates: total, sold: u0,
      profit-rate: profit-rate, maturity-block: (+ stacks-block-height maturity-blocks), status: "open"
    })
    (var-set issuance-count id) (ok id)))

(define-public (buy-sukuk (issuance-id uint) (quantity uint))
  (let (
    (sukuk (unwrap! (map-get? sukuk-issuances issuance-id) ERR-NOT-FOUND))
    (cost (* quantity (get face-value sukuk)))
    (prev (default-to { certificates: u0, invested: u0, profit-claimed: u0 } (map-get? holders tx-sender)))
  )
    (asserts! (is-eq (get status sukuk) "open") ERR-MATURED)
    (asserts! (<= (+ (get sold sukuk) quantity) (get total-certificates sukuk)) ERR-INSUFFICIENT)
    (try! (stx-transfer? cost tx-sender CONTRACT-OWNER))
    (try! (ft-mint? sukuk-cert quantity tx-sender))
    (map-set holders tx-sender { certificates: (+ (get certificates prev) quantity), invested: (+ (get invested prev) cost), profit-claimed: (get profit-claimed prev) })
    (map-set sukuk-issuances issuance-id (merge sukuk { sold: (+ (get sold sukuk) quantity) }))
    (var-set total-issued (+ (var-get total-issued) cost))
    (ok cost)))

(define-public (distribute-profit (holder principal) (amount uint))
  (let ((h (unwrap! (map-get? holders holder) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? amount tx-sender holder))
    (map-set holders holder (merge h { profit-claimed: (+ (get profit-claimed h) amount) }))
    (var-set total-profit-paid (+ (var-get total-profit-paid) amount))
    (ok amount)))

(define-public (redeem-sukuk (quantity uint))
  (let ((h (unwrap! (map-get? holders tx-sender) ERR-NOT-FOUND)))
    (asserts! (>= (get certificates h) quantity) ERR-INSUFFICIENT)
    (try! (ft-burn? sukuk-cert quantity tx-sender))
    (map-set holders tx-sender (merge h { certificates: (- (get certificates h) quantity) }))
    (ok quantity)))

(define-read-only (get-issuance (id uint)) (map-get? sukuk-issuances id))
(define-read-only (get-holder (who principal)) (map-get? holders who))
(define-read-only (get-issuance-count) (ok (var-get issuance-count)))
(define-read-only (get-total-issued) (ok (var-get total-issued)))
(define-read-only (get-sukuk-balance (who principal)) (ok (ft-get-balance sukuk-cert who)))
