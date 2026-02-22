;; Auction House Contract
;; Ascending bid auction - halal trade mechanism
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-AUCTION-NOT-FOUND (err u404))
(define-constant ERR-AUCTION-ENDED (err u405))
(define-constant ERR-BID-TOO-LOW (err u406))
(define-constant ERR-AUCTION-ACTIVE (err u407))
(define-constant ERR-SELF-BID (err u408))

(define-data-var auction-count uint u0)

(define-map auctions uint {
  seller: principal,
  title: (string-utf8 100),
  min-price: uint,
  highest-bid: uint,
  highest-bidder: (optional principal),
  end-block: uint,
  settled: bool
})
(define-map bids { auction-id: uint, bidder: principal } uint)

(define-public (create-auction (title (string-utf8 100)) (min-price uint) (duration uint))
  (let ((id (+ (var-get auction-count) u1)))
    (map-set auctions id {
      seller: tx-sender, title: title, min-price: min-price,
      highest-bid: u0, highest-bidder: none,
      end-block: (+ stacks-block-height duration), settled: false
    })
    (var-set auction-count id)
    (ok id)))

(define-public (place-bid (auction-id uint) (amount uint))
  (let ((auction (unwrap! (map-get? auctions auction-id) ERR-AUCTION-NOT-FOUND)))
    (asserts! (< stacks-block-height (get end-block auction)) ERR-AUCTION-ENDED)
    (asserts! (not (is-eq tx-sender (get seller auction))) ERR-SELF-BID)
    (asserts! (> amount (get highest-bid auction)) ERR-BID-TOO-LOW)
    (asserts! (>= amount (get min-price auction)) ERR-BID-TOO-LOW)
    (try! (stx-transfer? amount tx-sender (get seller auction)))
    (map-set bids { auction-id: auction-id, bidder: tx-sender } amount)
    (map-set auctions auction-id (merge auction {
      highest-bid: amount, highest-bidder: (some tx-sender)
    }))
    (print { event: "new-bid", auction: auction-id, bidder: tx-sender, amount: amount })
    (ok amount)))

(define-public (settle-auction (auction-id uint))
  (let ((auction (unwrap! (map-get? auctions auction-id) ERR-AUCTION-NOT-FOUND)))
    (asserts! (>= stacks-block-height (get end-block auction)) ERR-AUCTION-ACTIVE)
    (asserts! (not (get settled auction)) ERR-AUCTION-ENDED)
    (map-set auctions auction-id (merge auction { settled: true }))
    (print { event: "settled", auction: auction-id, winner: (get highest-bidder auction), price: (get highest-bid auction) })
    (ok true)))

(define-read-only (get-auction (id uint)) (map-get? auctions id))
(define-read-only (get-auction-count) (ok (var-get auction-count)))
(define-read-only (get-bid (auction-id uint) (bidder principal))
  (ok (default-to u0 (map-get? bids { auction-id: auction-id, bidder: bidder }))))
