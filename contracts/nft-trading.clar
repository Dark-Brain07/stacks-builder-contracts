;; NFT Trading Contract
;; Peer-to-peer NFT buy/sell platform
;; Halal - legitimate asset trade
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-LISTED (err u405))
(define-constant PLATFORM-FEE-PCT u2) ;; 2%

(define-data-var listing-count uint u0)
(define-data-var total-trades uint u0)
(define-data-var total-volume uint u0)

(define-map listings uint {
  seller: principal, nft-id: uint, collection: (string-utf8 100),
  price: uint, description: (string-utf8 200), active: bool, listed: uint
})
(define-map trade-history uint { listing-id: uint, buyer: principal, price: uint, block: uint })

(define-public (list-nft (nft-id uint) (collection (string-utf8 100)) (price uint) (description (string-utf8 200)))
  (let ((id (+ (var-get listing-count) u1)))
    (map-set listings id {
      seller: tx-sender, nft-id: nft-id, collection: collection,
      price: price, description: description, active: true, listed: stacks-block-height
    })
    (var-set listing-count id) (ok id)))

(define-public (update-price (listing-id uint) (new-price uint))
  (let ((listing (unwrap! (map-get? listings listing-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get seller listing)) ERR-NOT-AUTHORIZED)
    (map-set listings listing-id (merge listing { price: new-price })) (ok true)))

(define-public (buy-nft (listing-id uint))
  (let (
    (listing (unwrap! (map-get? listings listing-id) ERR-NOT-FOUND))
    (fee (/ (* (get price listing) PLATFORM-FEE-PCT) u100))
    (seller-amount (- (get price listing) fee))
    (trade-id (+ (var-get total-trades) u1))
  )
    (asserts! (get active listing) ERR-NOT-FOUND)
    (try! (stx-transfer? seller-amount tx-sender (get seller listing)))
    (try! (stx-transfer? fee tx-sender CONTRACT-OWNER))
    (map-set listings listing-id (merge listing { active: false }))
    (map-set trade-history trade-id { listing-id: listing-id, buyer: tx-sender, price: (get price listing), block: stacks-block-height })
    (var-set total-trades trade-id)
    (var-set total-volume (+ (var-get total-volume) (get price listing)))
    (ok (get price listing))))

(define-public (cancel-listing (listing-id uint))
  (let ((listing (unwrap! (map-get? listings listing-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get seller listing)) ERR-NOT-AUTHORIZED)
    (map-set listings listing-id (merge listing { active: false })) (ok true)))

(define-read-only (get-listing (id uint)) (map-get? listings id))
(define-read-only (get-trade (id uint)) (map-get? trade-history id))
(define-read-only (get-listing-count) (ok (var-get listing-count)))
(define-read-only (get-total-trades) (ok (var-get total-trades)))
(define-read-only (get-total-volume) (ok (var-get total-volume)))
