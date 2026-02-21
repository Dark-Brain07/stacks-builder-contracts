;; Escrow Service Contract
;; Trustless escrow for peer-to-peer transactions
;; Built by rajuice for Stacks Builder Rewards

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-ESCROW-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-RELEASED (err u405))
(define-constant ERR-INVALID-STATE (err u406))
(define-constant ERR-DISPUTE-ACTIVE (err u407))

(define-data-var escrow-count uint u0)

(define-map escrows uint {
  buyer: principal,
  seller: principal,
  amount: uint,
  released: bool,
  disputed: bool,
  created-at: uint
})

(define-public (create-escrow (seller principal) (amount uint))
  (let ((escrow-id (+ (var-get escrow-count) u1)))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set escrows escrow-id {
      buyer: tx-sender, seller: seller, amount: amount,
      released: false, disputed: false, created-at: stacks-block-height
    })
    (var-set escrow-count escrow-id)
    (ok escrow-id)))

(define-public (release-escrow (escrow-id uint))
  (let ((escrow (unwrap! (map-get? escrows escrow-id) ERR-ESCROW-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get buyer escrow)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get released escrow)) ERR-ALREADY-RELEASED)
    (asserts! (not (get disputed escrow)) ERR-DISPUTE-ACTIVE)
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get seller escrow))))
    (map-set escrows escrow-id (merge escrow { released: true }))
    (ok true)))

(define-public (refund-escrow (escrow-id uint))
  (let ((escrow (unwrap! (map-get? escrows escrow-id) ERR-ESCROW-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get seller escrow)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get released escrow)) ERR-ALREADY-RELEASED)
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get buyer escrow))))
    (map-set escrows escrow-id (merge escrow { released: true }))
    (ok true)))

(define-public (dispute-escrow (escrow-id uint))
  (let ((escrow (unwrap! (map-get? escrows escrow-id) ERR-ESCROW-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender (get buyer escrow)) (is-eq tx-sender (get seller escrow))) ERR-NOT-AUTHORIZED)
    (asserts! (not (get released escrow)) ERR-ALREADY-RELEASED)
    (map-set escrows escrow-id (merge escrow { disputed: true }))
    (ok true)))

(define-public (resolve-dispute (escrow-id uint) (pay-seller bool))
  (let ((escrow (unwrap! (map-get? escrows escrow-id) ERR-ESCROW-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (get disputed escrow) ERR-INVALID-STATE)
    (if pay-seller
      (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get seller escrow))))
      (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get buyer escrow)))))
    (map-set escrows escrow-id (merge escrow { released: true, disputed: false }))
    (ok true)))

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows escrow-id))

(define-read-only (get-escrow-count)
  (ok (var-get escrow-count)))
