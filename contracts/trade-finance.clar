;; Trade Finance Contract
;; Letter of credit / trade facilitation
;; Halal - facilitating legitimate trade
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-RELEASED (err u405))

(define-data-var lc-count uint u0)
(define-data-var total-facilitated uint u0)

(define-map letters-of-credit uint {
  buyer: principal, seller: principal, amount: uint,
  goods-description: (string-utf8 200), shipping-deadline: uint,
  documents-submitted: bool, goods-received: bool,
  status: (string-ascii 20), created: uint
})

(define-public (open-lc (seller principal) (amount uint) (goods (string-utf8 200)) (deadline uint))
  (let ((id (+ (var-get lc-count) u1)))
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set letters-of-credit id {
      buyer: tx-sender, seller: seller, amount: amount,
      goods-description: goods, shipping-deadline: (+ stacks-block-height deadline),
      documents-submitted: false, goods-received: false,
      status: "open", created: stacks-block-height
    })
    (var-set lc-count id)
    (var-set total-facilitated (+ (var-get total-facilitated) amount))
    (ok id)))

(define-public (submit-documents (lc-id uint))
  (let ((lc (unwrap! (map-get? letters-of-credit lc-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get seller lc)) ERR-NOT-AUTHORIZED)
    (map-set letters-of-credit lc-id (merge lc { documents-submitted: true, status: "docs-submitted" }))
    (ok true)))

(define-public (confirm-receipt (lc-id uint))
  (let ((lc (unwrap! (map-get? letters-of-credit lc-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get buyer lc)) ERR-NOT-AUTHORIZED)
    (map-set letters-of-credit lc-id (merge lc { goods-received: true, status: "goods-received" }))
    (ok true)))

(define-public (release-payment (lc-id uint))
  (let ((lc (unwrap! (map-get? letters-of-credit lc-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (get documents-submitted lc) ERR-NOT-FOUND)
    (asserts! (not (is-eq (get status lc) "released")) ERR-ALREADY-RELEASED)
    (try! (stx-transfer? (get amount lc) tx-sender (get seller lc)))
    (map-set letters-of-credit lc-id (merge lc { status: "released" }))
    (ok (get amount lc))))

(define-public (dispute-lc (lc-id uint))
  (let ((lc (unwrap! (map-get? letters-of-credit lc-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get buyer lc)) ERR-NOT-AUTHORIZED)
    (map-set letters-of-credit lc-id (merge lc { status: "disputed" })) (ok true)))

(define-read-only (get-lc (id uint)) (map-get? letters-of-credit id))
(define-read-only (get-lc-count) (ok (var-get lc-count)))
(define-read-only (get-total-facilitated) (ok (var-get total-facilitated)))
