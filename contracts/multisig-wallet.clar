;; Multi-Signature Wallet Contract
;; Requires multiple signers to approve transactions
;; Built by rajuice for Stacks Builder Rewards

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-ALREADY-SIGNED (err u402))
(define-constant ERR-TX-NOT-FOUND (err u404))
(define-constant ERR-TX-EXECUTED (err u405))
(define-constant ERR-NOT-ENOUGH-SIGS (err u406))
(define-constant ERR-NOT-SIGNER (err u407))
(define-constant REQUIRED-SIGS u2)

(define-data-var tx-count uint u0)

(define-map signers principal bool)
(define-map transactions uint {
  to: principal,
  amount: uint,
  sigs: uint,
  executed: bool,
  creator: principal
})
(define-map has-signed { tx-id: uint, signer: principal } bool)

;; Initialize signers
(map-set signers CONTRACT-OWNER true)

(define-public (add-signer (signer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set signers signer true)
    (ok true)))

(define-public (remove-signer (signer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-delete signers signer)
    (ok true)))

(define-public (submit-tx (to principal) (amount uint))
  (let ((tx-id (+ (var-get tx-count) u1)))
    (asserts! (is-signer tx-sender) ERR-NOT-SIGNER)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set transactions tx-id {
      to: to, amount: amount, sigs: u1, executed: false, creator: tx-sender
    })
    (map-set has-signed { tx-id: tx-id, signer: tx-sender } true)
    (var-set tx-count tx-id)
    (ok tx-id)))

(define-public (sign-tx (tx-id uint))
  (let ((tx (unwrap! (map-get? transactions tx-id) ERR-TX-NOT-FOUND)))
    (asserts! (is-signer tx-sender) ERR-NOT-SIGNER)
    (asserts! (not (get executed tx)) ERR-TX-EXECUTED)
    (asserts! (is-none (map-get? has-signed { tx-id: tx-id, signer: tx-sender })) ERR-ALREADY-SIGNED)
    (map-set has-signed { tx-id: tx-id, signer: tx-sender } true)
    (map-set transactions tx-id (merge tx { sigs: (+ (get sigs tx) u1) }))
    (ok (+ (get sigs tx) u1))))

(define-public (execute-tx (tx-id uint))
  (let ((tx (unwrap! (map-get? transactions tx-id) ERR-TX-NOT-FOUND)))
    (asserts! (not (get executed tx)) ERR-TX-EXECUTED)
    (asserts! (>= (get sigs tx) REQUIRED-SIGS) ERR-NOT-ENOUGH-SIGS)
    (try! (as-contract (stx-transfer? (get amount tx) tx-sender (get to tx))))
    (map-set transactions tx-id (merge tx { executed: true }))
    (ok true)))

(define-private (is-signer (account principal))
  (default-to false (map-get? signers account)))

(define-read-only (get-tx (tx-id uint))
  (map-get? transactions tx-id))

(define-read-only (get-tx-count)
  (ok (var-get tx-count)))

(define-read-only (check-signer (account principal))
  (ok (is-signer account)))
