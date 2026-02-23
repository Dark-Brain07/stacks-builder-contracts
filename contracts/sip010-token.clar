;; SIP-010 Fungible Token Implementation
;; A fungible token for the Stacks blockchain
;; Built by rajuice for Stacks Builder Rewards

;; Define the fungible token
(define-fungible-token builder-token)

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-INSUFFICIENT-BALANCE (err u402))
(define-constant ERR-INVALID-AMOUNT (err u403))

;; Data vars
(define-data-var token-uri (optional (string-utf8 256)) (some u"https://stacks-builder-token.com/metadata.json"))
(define-data-var total-minted uint u0)

;; Transfer tokens from sender to recipient
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (try! (ft-transfer? builder-token amount sender recipient))
    (match memo to-print (print to-print) 0x)
    (ok true)))

;; Get the token name
(define-read-only (get-name)
  (ok "Stacks Builder Token"))

;; Get the token symbol
(define-read-only (get-symbol)
  (ok "SBT"))

;; Get the number of decimals
(define-read-only (get-decimals)
  (ok u6))

;; Get the balance of an account
(define-read-only (get-balance (account principal))
  (ok (ft-get-balance builder-token account)))

;; Get the total supply
(define-read-only (get-total-supply)
  (ok (ft-get-supply builder-token)))

;; Get the token URI
(define-read-only (get-token-uri)
  (ok (var-get token-uri)))

;; Mint new tokens (owner only)
(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (var-set total-minted (+ (var-get total-minted) amount))
    (ft-mint? builder-token amount recipient)))

;; Burn tokens
(define-public (burn (amount uint))
  (begin
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (ft-burn? builder-token amount tx-sender)))

;; Update token URI (owner only)
(define-public (set-token-uri (new-uri (optional (string-utf8 256))))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set token-uri new-uri)
    (ok true)))

;; Get total tokens ever minted
(define-read-only (get-total-minted)
  (ok (var-get total-minted)))

;; Check if sender is the contract owner
(define-read-only (is-owner (account principal))
  (is-eq account CONTRACT-OWNER))
