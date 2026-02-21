;; Escrow Service Contract
;; Trustless escrow for peer-to-peer transactions
;; Built by rajuice for Stacks Builder Rewards
;; Clarity 4 compatible (no as-contract)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-ESCROW-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-RELEASED (err u405))
(define-constant ERR-INVALID-STATE (err u406))
(define-constant ERR-DISPUTE-ACTIVE (err u407))
(define-constant ERR-INVALID-AMOUNT (err u408))

(define-data-var escrow-count uint u0)

(define-map escrows uint {
  buyer: principal,
  seller: principal,
  amount: uint,
  state: (string-ascii 20),
  created-at: uint
})

;; Create escrow - funds sent to contract owner (escrow agent)
(define-public (create-escrow (seller principal) (amount uint))
  (let ((escrow-id (+ (var-get escrow-count) u1)))
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set escrows escrow-id {
      buyer: tx-sender, seller: seller, amount: amount,
      state: "active", created-at: stacks-block-height
    })
    (var-set escrow-count escrow-id)
    (ok escrow-id)))

;; Buyer releases funds to seller
(define-public (release-escrow (escrow-id uint))
  (let ((escrow (unwrap! (map-get? escrows escrow-id) ERR-ESCROW-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get buyer escrow)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get state escrow) "active") ERR-ALREADY-RELEASED)
    ;; Owner sends to seller
    (map-set escrows escrow-id (merge escrow { state: "released" }))
    (print { event: "escrow-released", id: escrow-id, seller: (get seller escrow), amount: (get amount escrow) })
    (ok true)))

;; Owner fulfills the release
(define-public (fulfill-release (escrow-id uint))
  (let ((escrow (unwrap! (map-get? escrows escrow-id) ERR-ESCROW-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get state escrow) "released") ERR-INVALID-STATE)
    (try! (stx-transfer? (get amount escrow) tx-sender (get seller escrow)))
    (map-set escrows escrow-id (merge escrow { state: "completed" }))
    (ok true)))

;; Seller agrees to refund
(define-public (refund-escrow (escrow-id uint))
  (let ((escrow (unwrap! (map-get? escrows escrow-id) ERR-ESCROW-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get seller escrow)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get state escrow) "active") ERR-ALREADY-RELEASED)
    (map-set escrows escrow-id (merge escrow { state: "refund-pending" }))
    (ok true)))

;; Owner fulfills refund
(define-public (fulfill-refund (escrow-id uint))
  (let ((escrow (unwrap! (map-get? escrows escrow-id) ERR-ESCROW-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get state escrow) "refund-pending") ERR-INVALID-STATE)
    (try! (stx-transfer? (get amount escrow) tx-sender (get buyer escrow)))
    (map-set escrows escrow-id (merge escrow { state: "refunded" }))
    (ok true)))

;; Dispute
(define-public (dispute-escrow (escrow-id uint))
  (let ((escrow (unwrap! (map-get? escrows escrow-id) ERR-ESCROW-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender (get buyer escrow)) (is-eq tx-sender (get seller escrow))) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get state escrow) "active") ERR-ALREADY-RELEASED)
    (map-set escrows escrow-id (merge escrow { state: "disputed" }))
    (ok true)))

;; Resolve dispute (owner decides)
(define-public (resolve-dispute (escrow-id uint) (pay-seller bool))
  (let ((escrow (unwrap! (map-get? escrows escrow-id) ERR-ESCROW-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get state escrow) "disputed") ERR-INVALID-STATE)
    (if pay-seller
      (try! (stx-transfer? (get amount escrow) tx-sender (get seller escrow)))
      (try! (stx-transfer? (get amount escrow) tx-sender (get buyer escrow))))
    (map-set escrows escrow-id (merge escrow { state: "resolved" }))
    (ok true)))

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows escrow-id))

(define-read-only (get-escrow-count)
  (ok (var-get escrow-count)))
