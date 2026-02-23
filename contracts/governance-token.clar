;; Governance Token Contract
;; Fungible governance token for DAO voting power
;; Halal - organizational governance
;; Clarity 4 compatible

(define-fungible-token gov-token)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-INSUFFICIENT (err u402))
(define-constant MAX-SUPPLY u1000000000000) ;; 1M tokens (6 decimals)

(define-data-var token-uri (optional (string-utf8 200)) none)
(define-data-var minting-enabled bool true)

(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (var-get minting-enabled) ERR-NOT-AUTHORIZED)
    (asserts! (<= (+ (ft-get-supply gov-token) amount) MAX-SUPPLY) ERR-INSUFFICIENT)
    (ft-mint? gov-token amount recipient)))

(define-public (burn (amount uint))
  (begin
    (asserts! (>= (ft-get-balance gov-token tx-sender) amount) ERR-INSUFFICIENT)
    (ft-burn? gov-token amount tx-sender)))

(define-public (transfer (amount uint) (to principal) (memo (optional (buff 34))))
  (begin
    (try! (ft-transfer? gov-token amount tx-sender to))
    (match memo m (print m) 0x)
    (ok true)))

(define-public (set-token-uri (uri (string-utf8 200)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set token-uri (some uri)) (ok true)))

(define-public (toggle-minting)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set minting-enabled (not (var-get minting-enabled))) (ok (var-get minting-enabled))))

(define-read-only (get-name) (ok "Governance Token"))
(define-read-only (get-symbol) (ok "GOV"))
(define-read-only (get-decimals) (ok u6))
(define-read-only (get-balance (who principal)) (ok (ft-get-balance gov-token who)))
(define-read-only (get-total-supply) (ok (ft-get-supply gov-token)))
(define-read-only (get-token-uri) (ok (var-get token-uri)))
(define-read-only (get-max-supply) (ok MAX-SUPPLY))
(define-read-only (is-minting-enabled) (ok (var-get minting-enabled)))
