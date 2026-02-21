;; Name Service Contract
;; Register and manage .stx names
;; Built by rajuice for Stacks Builder Rewards

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NAME-TAKEN (err u402))
(define-constant ERR-NAME-NOT-FOUND (err u404))
(define-constant ERR-NAME-EXPIRED (err u405))
(define-constant REGISTRATION-FEE u2000000) ;; 2 STX
(define-constant RENEWAL-PERIOD u52560) ;; ~1 year in blocks

(define-data-var total-names uint u0)

(define-map names (string-ascii 48) {
  owner: principal,
  resolver: principal,
  registered-at: uint,
  expires-at: uint
})

(define-map reverse-lookup principal (string-ascii 48))

(define-public (register-name (name (string-ascii 48)) (resolver principal))
  (begin
    (asserts! (is-none (map-get? names name)) ERR-NAME-TAKEN)
    (try! (stx-transfer? REGISTRATION-FEE tx-sender (as-contract tx-sender)))
    (map-set names name {
      owner: tx-sender,
      resolver: resolver,
      registered-at: stacks-block-height,
      expires-at: (+ stacks-block-height RENEWAL-PERIOD)
    })
    (map-set reverse-lookup tx-sender name)
    (var-set total-names (+ (var-get total-names) u1))
    (ok true)))

(define-public (renew-name (name (string-ascii 48)))
  (let ((entry (unwrap! (map-get? names name) ERR-NAME-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner entry)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? REGISTRATION-FEE tx-sender (as-contract tx-sender)))
    (map-set names name (merge entry { expires-at: (+ (get expires-at entry) RENEWAL-PERIOD) }))
    (ok true)))

(define-public (transfer-name (name (string-ascii 48)) (new-owner principal))
  (let ((entry (unwrap! (map-get? names name) ERR-NAME-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner entry)) ERR-NOT-AUTHORIZED)
    (asserts! (< stacks-block-height (get expires-at entry)) ERR-NAME-EXPIRED)
    (map-set names name (merge entry { owner: new-owner }))
    (map-delete reverse-lookup tx-sender)
    (map-set reverse-lookup new-owner name)
    (ok true)))

(define-public (update-resolver (name (string-ascii 48)) (new-resolver principal))
  (let ((entry (unwrap! (map-get? names name) ERR-NAME-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner entry)) ERR-NOT-AUTHORIZED)
    (map-set names name (merge entry { resolver: new-resolver }))
    (ok true)))

(define-read-only (resolve-name (name (string-ascii 48)))
  (let ((entry (map-get? names name)))
    (match entry
      e (if (< stacks-block-height (get expires-at e))
          (ok (get resolver e))
          ERR-NAME-EXPIRED)
      ERR-NAME-NOT-FOUND)))

(define-read-only (get-name-info (name (string-ascii 48)))
  (map-get? names name))

(define-read-only (get-name-by-address (addr principal))
  (map-get? reverse-lookup addr))

(define-read-only (get-total-names)
  (ok (var-get total-names)))

(define-read-only (get-registration-fee)
  (ok REGISTRATION-FEE))
