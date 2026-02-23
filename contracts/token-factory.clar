;; Token Factory Contract
;; Create and manage custom token namespaces
;; Halal - legitimate token creation
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-EXISTS (err u405))
(define-constant ERR-EXCEEDED-CAP (err u406))
(define-constant CREATION-FEE u100000) ;; 0.1 STX

(define-data-var token-count uint u0)
(define-data-var total-fees uint u0)

(define-map token-defs uint {
  creator: principal, name: (string-utf8 100), symbol: (string-ascii 10),
  max-supply: uint, minted: uint, decimals: uint, transferable: bool, created: uint
})
(define-map token-names (string-ascii 10) uint)
(define-map token-balances { token-id: uint, holder: principal } uint)
(define-map token-holders { token-id: uint, index: uint } principal)
(define-map holder-count uint uint)

(define-public (create-token (name (string-utf8 100)) (symbol (string-ascii 10)) (max-supply uint) (decimals uint))
  (let ((id (+ (var-get token-count) u1)))
    (asserts! (is-none (map-get? token-names symbol)) ERR-ALREADY-EXISTS)
    (try! (stx-transfer? CREATION-FEE tx-sender CONTRACT-OWNER))
    (map-set token-defs id { creator: tx-sender, name: name, symbol: symbol, max-supply: max-supply, minted: u0, decimals: decimals, transferable: true, created: stacks-block-height })
    (map-set token-names symbol id)
    (var-set token-count id)
    (var-set total-fees (+ (var-get total-fees) CREATION-FEE))
    (ok id)))

(define-public (mint-token (token-id uint) (amount uint) (to principal))
  (let (
    (td (unwrap! (map-get? token-defs token-id) ERR-NOT-FOUND))
    (current-bal (default-to u0 (map-get? token-balances { token-id: token-id, holder: to })))
    (is-new (is-eq current-bal u0))
  )
    (asserts! (is-eq tx-sender (get creator td)) ERR-NOT-AUTHORIZED)
    (asserts! (<= (+ (get minted td) amount) (get max-supply td)) ERR-EXCEEDED-CAP)
    (map-set token-balances { token-id: token-id, holder: to } (+ current-bal amount))
    (map-set token-defs token-id (merge td { minted: (+ (get minted td) amount) }))
    (if is-new
      (let ((idx (default-to u0 (map-get? holder-count token-id))))
        (map-set token-holders { token-id: token-id, index: idx } to)
        (map-set holder-count token-id (+ idx u1)) true) true)
    (ok amount)))

(define-public (transfer-token (token-id uint) (amount uint) (to principal))
  (let (
    (td (unwrap! (map-get? token-defs token-id) ERR-NOT-FOUND))
    (from-bal (default-to u0 (map-get? token-balances { token-id: token-id, holder: tx-sender })))
    (to-bal (default-to u0 (map-get? token-balances { token-id: token-id, holder: to })))
  )
    (asserts! (get transferable td) ERR-NOT-AUTHORIZED)
    (asserts! (>= from-bal amount) ERR-NOT-FOUND)
    (map-set token-balances { token-id: token-id, holder: tx-sender } (- from-bal amount))
    (map-set token-balances { token-id: token-id, holder: to } (+ to-bal amount))
    (ok amount)))

(define-read-only (get-token (id uint)) (map-get? token-defs id))
(define-read-only (get-token-by-symbol (symbol (string-ascii 10))) (map-get? token-names symbol))
(define-read-only (get-token-balance (token-id uint) (holder principal)) (ok (default-to u0 (map-get? token-balances { token-id: token-id, holder: holder }))))
(define-read-only (get-token-count) (ok (var-get token-count)))
(define-read-only (get-total-fees) (ok (var-get total-fees)))
