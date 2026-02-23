;; Multi-Signature Wallet Contract
;; Requires multiple signers to approve transactions
;; Built by rajuice for Stacks Builder Rewards
;; Clarity 4 compatible (no as-contract)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-ALREADY-SIGNED (err u402))
(define-constant ERR-TX-NOT-FOUND (err u404))
(define-constant ERR-TX-EXECUTED (err u405))
(define-constant ERR-NOT-ENOUGH-SIGS (err u406))
(define-constant ERR-NOT-SIGNER (err u407))
(define-constant ERR-INVALID-AMOUNT (err u408))
(define-constant REQUIRED-SIGS u2)

(define-data-var tx-count uint u0)
(define-data-var signer-count uint u1)

(define-map signers principal bool)
(define-map transactions uint {
  to: principal,
  amount: uint,
  sigs: uint,
  executed: bool,
  creator: principal
})
(define-map has-signed { tx-id: uint, signer: principal } bool)
(define-map deposits principal uint)

;; Initialize owner as signer
(map-set signers CONTRACT-OWNER true)

(define-public (add-signer (signer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set signers signer true)
    (var-set signer-count (+ (var-get signer-count) u1))
    (ok true)))

(define-public (remove-signer (signer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-delete signers signer)
    (var-set signer-count (- (var-get signer-count) u1))
    (ok true)))

;; Submit a new transaction proposal
(define-public (submit-tx (to principal) (amount uint))
  (let ((tx-id (+ (var-get tx-count) u1)))
    (asserts! (is-signer tx-sender) ERR-NOT-SIGNER)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (map-set transactions tx-id {
      to: to, amount: amount, sigs: u1, executed: false, creator: tx-sender
    })
    (map-set has-signed { tx-id: tx-id, signer: tx-sender } true)
    (var-set tx-count tx-id)
    (ok tx-id)))

;; Sign a pending transaction
(define-public (sign-tx (tx-id uint))
  (let ((tx (unwrap! (map-get? transactions tx-id) ERR-TX-NOT-FOUND)))
    (asserts! (is-signer tx-sender) ERR-NOT-SIGNER)
    (asserts! (not (get executed tx)) ERR-TX-EXECUTED)
    (asserts! (is-none (map-get? has-signed { tx-id: tx-id, signer: tx-sender })) ERR-ALREADY-SIGNED)
    (map-set has-signed { tx-id: tx-id, signer: tx-sender } true)
    (map-set transactions tx-id (merge tx { sigs: (+ (get sigs tx) u1) }))
    (ok (+ (get sigs tx) u1))))

;; Execute an approved transaction
(define-public (execute-tx (tx-id uint))
  (let ((tx (unwrap! (map-get? transactions tx-id) ERR-TX-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (not (get executed tx)) ERR-TX-EXECUTED)
    (asserts! (>= (get sigs tx) REQUIRED-SIGS) ERR-NOT-ENOUGH-SIGS)
    ;; Owner executes the transfer after sufficient signatures
    (try! (stx-transfer? (get amount tx) tx-sender (get to tx)))
    (map-set transactions tx-id (merge tx { executed: true }))
    (ok true)))

;; Deposit funds to track contributions
(define-public (deposit (amount uint))
  (let ((current (default-to u0 (map-get? deposits tx-sender))))
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set deposits tx-sender (+ current amount))
    (ok (+ current amount))))

(define-private (is-signer (account principal))
  (default-to false (map-get? signers account)))

(define-read-only (get-tx (tx-id uint))
  (map-get? transactions tx-id))

(define-read-only (get-tx-count)
  (ok (var-get tx-count)))

(define-read-only (check-signer (account principal))
  (ok (is-signer account)))

(define-read-only (get-signer-count)
  (ok (var-get signer-count)))

(define-read-only (get-deposit (account principal))
  (ok (default-to u0 (map-get? deposits account))))
