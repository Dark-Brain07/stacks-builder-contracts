;; SIP-009 Non-Fungible Token Implementation
;; NFT contract for the Stacks blockchain
;; Built by rajuice for Stacks Builder Rewards

;; Define the NFT
(define-non-fungible-token stacks-builder-nft uint)

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-MAX-SUPPLY-REACHED (err u410))
(define-constant ERR-LISTING-NOT-FOUND (err u411))

;; Maximum supply
(define-constant MAX-SUPPLY u10000)

;; Data vars
(define-data-var last-token-id uint u0)
(define-data-var base-uri (string-utf8 256) u"https://stacks-builder-nft.com/metadata/")
(define-data-var mint-price uint u1000000)
(define-data-var minting-enabled bool true)

;; Data maps
(define-map token-metadata uint (string-utf8 256))
(define-map token-listings uint uint)

;; Get the last minted token ID
(define-read-only (get-last-token-id)
  (ok (var-get last-token-id)))

;; Get the token URI for a specific token
(define-read-only (get-token-uri (token-id uint))
  (ok (some (var-get base-uri))))

;; Get the owner of a specific token
(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? stacks-builder-nft token-id)))

;; Transfer an NFT
(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (nft-get-owner? stacks-builder-nft token-id)) ERR-NOT-FOUND)
    (map-delete token-listings token-id)
    (nft-transfer? stacks-builder-nft token-id sender recipient)))

;; Mint a new NFT (public minting with payment)
(define-public (mint)
  (let (
    (token-id (+ (var-get last-token-id) u1))
    (price (var-get mint-price))
  )
    (asserts! (var-get minting-enabled) ERR-NOT-AUTHORIZED)
    (asserts! (<= token-id MAX-SUPPLY) ERR-MAX-SUPPLY-REACHED)
    (try! (stx-transfer? price tx-sender CONTRACT-OWNER))
    (try! (nft-mint? stacks-builder-nft token-id tx-sender))
    (var-set last-token-id token-id)
    (ok token-id)))

;; Owner mint (free, for airdrops)
(define-public (mint-for (recipient principal))
  (let ((token-id (+ (var-get last-token-id) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= token-id MAX-SUPPLY) ERR-MAX-SUPPLY-REACHED)
    (try! (nft-mint? stacks-builder-nft token-id recipient))
    (var-set last-token-id token-id)
    (ok token-id)))

;; Burn an NFT
(define-public (burn (token-id uint))
  (begin
    (asserts! (is-eq (some tx-sender) (nft-get-owner? stacks-builder-nft token-id)) ERR-NOT-AUTHORIZED)
    (map-delete token-listings token-id)
    (nft-burn? stacks-builder-nft token-id tx-sender)))

;; Set base URI (owner only)
(define-public (set-base-uri (new-uri (string-utf8 256)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set base-uri new-uri)
    (ok true)))

;; Set mint price (owner only)
(define-public (set-mint-price (new-price uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set mint-price new-price)
    (ok true)))

;; Toggle minting (owner only)
(define-public (toggle-minting)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set minting-enabled (not (var-get minting-enabled)))
    (ok (var-get minting-enabled))))

;; List NFT for sale
(define-public (list-for-sale (token-id uint) (price uint))
  (begin
    (asserts! (is-eq (some tx-sender) (nft-get-owner? stacks-builder-nft token-id)) ERR-NOT-AUTHORIZED)
    (map-set token-listings token-id price)
    (ok true)))

;; Remove from sale
(define-public (unlist (token-id uint))
  (begin
    (asserts! (is-eq (some tx-sender) (nft-get-owner? stacks-builder-nft token-id)) ERR-NOT-AUTHORIZED)
    (map-delete token-listings token-id)
    (ok true)))

;; Buy listed NFT
(define-public (buy (token-id uint))
  (let (
    (listing-price (unwrap! (map-get? token-listings token-id) ERR-LISTING-NOT-FOUND))
    (owner (unwrap! (nft-get-owner? stacks-builder-nft token-id) ERR-NOT-FOUND))
  )
    (try! (stx-transfer? listing-price tx-sender owner))
    (try! (nft-transfer? stacks-builder-nft token-id owner tx-sender))
    (map-delete token-listings token-id)
    (ok true)))

;; Get listing price
(define-read-only (get-listing-price (token-id uint))
  (map-get? token-listings token-id))

;; Read-only getters
(define-read-only (get-mint-price)
  (ok (var-get mint-price)))

(define-read-only (get-max-supply)
  (ok MAX-SUPPLY))

(define-read-only (is-minting-enabled)
  (ok (var-get minting-enabled)))

(define-read-only (get-remaining-supply)
  (ok (- MAX-SUPPLY (var-get last-token-id))))
