;; Charity Auction Contract
;; Auction items for charitable causes
;; Halal - transparent charity auctions
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-BID-TOO-LOW (err u405))
(define-constant ERR-ENDED (err u406))

(define-data-var auction-count uint u0)
(define-data-var total-charity uint u0)

(define-map auctions uint {
  seller: principal, item: (string-utf8 100), beneficiary: principal,
  min-bid: uint, highest-bid: uint, highest-bidder: (optional principal),
  end-block: uint, settled: bool, charity-pct: uint
})
(define-map bid-history { auction-id: uint, index: uint } { bidder: principal, amount: uint, block: uint })
(define-map bid-count uint uint)

(define-public (create-auction (item (string-utf8 100)) (beneficiary principal) (min-bid uint) (duration uint) (charity-pct uint))
  (let ((id (+ (var-get auction-count) u1)))
    (map-set auctions id { seller: tx-sender, item: item, beneficiary: beneficiary, min-bid: min-bid, highest-bid: u0, highest-bidder: none, end-block: (+ stacks-block-height duration), settled: false, charity-pct: charity-pct })
    (var-set auction-count id) (ok id)))

(define-public (place-bid (auction-id uint) (amount uint))
  (let (
    (auction (unwrap! (map-get? auctions auction-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? bid-count auction-id)))
  )
    (asserts! (< stacks-block-height (get end-block auction)) ERR-ENDED)
    (asserts! (> amount (get highest-bid auction)) ERR-BID-TOO-LOW)
    (asserts! (>= amount (get min-bid auction)) ERR-BID-TOO-LOW)
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    ;; Refund previous highest bidder
    (match (get highest-bidder auction)
      prev-bidder (try! (stx-transfer? (get highest-bid auction) CONTRACT-OWNER prev-bidder))
      true)
    (map-set bid-history { auction-id: auction-id, index: idx } { bidder: tx-sender, amount: amount, block: stacks-block-height })
    (map-set bid-count auction-id (+ idx u1))
    (map-set auctions auction-id (merge auction { highest-bid: amount, highest-bidder: (some tx-sender) }))
    (ok amount)))

(define-public (settle-auction (auction-id uint))
  (let (
    (auction (unwrap! (map-get? auctions auction-id) ERR-NOT-FOUND))
    (charity-amount (/ (* (get highest-bid auction) (get charity-pct auction)) u100))
    (seller-amount (- (get highest-bid auction) charity-amount))
  )
    (asserts! (>= stacks-block-height (get end-block auction)) ERR-NOT-FOUND)
    (asserts! (not (get settled auction)) ERR-ENDED)
    (if (> charity-amount u0) (try! (stx-transfer? charity-amount CONTRACT-OWNER (get beneficiary auction))) true)
    (if (> seller-amount u0) (try! (stx-transfer? seller-amount CONTRACT-OWNER (get seller auction))) true)
    (map-set auctions auction-id (merge auction { settled: true }))
    (var-set total-charity (+ (var-get total-charity) charity-amount))
    (ok charity-amount)))

(define-read-only (get-auction (id uint)) (map-get? auctions id))
(define-read-only (get-bid (auction-id uint) (index uint)) (map-get? bid-history { auction-id: auction-id, index: index }))
(define-read-only (get-auction-count) (ok (var-get auction-count)))
(define-read-only (get-total-charity) (ok (var-get total-charity)))
