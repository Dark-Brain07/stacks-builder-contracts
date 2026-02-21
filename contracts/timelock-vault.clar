;; Timelock Vault Contract
;; Lock STX for a specified duration before withdrawal
;; Built by rajuice for Stacks Builder Rewards

(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-VAULT-NOT-FOUND (err u404))
(define-constant ERR-STILL-LOCKED (err u405))
(define-constant ERR-ALREADY-WITHDRAWN (err u406))
(define-constant ERR-INVALID-DURATION (err u407))
(define-constant MIN-LOCK u10)

(define-data-var vault-count uint u0)

(define-map vaults uint {
  owner: principal,
  amount: uint,
  unlock-height: uint,
  withdrawn: bool,
  beneficiary: principal
})

(define-public (create-vault (amount uint) (lock-blocks uint) (beneficiary principal))
  (let ((vault-id (+ (var-get vault-count) u1)))
    (asserts! (>= lock-blocks MIN-LOCK) ERR-INVALID-DURATION)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set vaults vault-id {
      owner: tx-sender,
      amount: amount,
      unlock-height: (+ stacks-block-height lock-blocks),
      withdrawn: false,
      beneficiary: beneficiary
    })
    (var-set vault-count vault-id)
    (ok vault-id)))

(define-public (withdraw-vault (vault-id uint))
  (let ((vault (unwrap! (map-get? vaults vault-id) ERR-VAULT-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender (get owner vault)) (is-eq tx-sender (get beneficiary vault))) ERR-NOT-AUTHORIZED)
    (asserts! (>= stacks-block-height (get unlock-height vault)) ERR-STILL-LOCKED)
    (asserts! (not (get withdrawn vault)) ERR-ALREADY-WITHDRAWN)
    (try! (as-contract (stx-transfer? (get amount vault) tx-sender (get beneficiary vault))))
    (map-set vaults vault-id (merge vault { withdrawn: true }))
    (ok (get amount vault))))

(define-public (extend-lock (vault-id uint) (extra-blocks uint))
  (let ((vault (unwrap! (map-get? vaults vault-id) ERR-VAULT-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner vault)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get withdrawn vault)) ERR-ALREADY-WITHDRAWN)
    (map-set vaults vault-id (merge vault { unlock-height: (+ (get unlock-height vault) extra-blocks) }))
    (ok true)))

(define-read-only (get-vault (vault-id uint))
  (map-get? vaults vault-id))

(define-read-only (get-vault-count)
  (ok (var-get vault-count)))

(define-read-only (is-unlocked (vault-id uint))
  (let ((vault (map-get? vaults vault-id)))
    (match vault
      v (ok (>= stacks-block-height (get unlock-height v)))
      (err ERR-VAULT-NOT-FOUND))))

(define-read-only (get-time-remaining (vault-id uint))
  (let ((vault (map-get? vaults vault-id)))
    (match vault
      v (ok (if (>= stacks-block-height (get unlock-height v))
              u0
              (- (get unlock-height v) stacks-block-height)))
      (err ERR-VAULT-NOT-FOUND))))
