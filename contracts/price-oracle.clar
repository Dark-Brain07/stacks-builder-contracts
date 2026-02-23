;; Price Oracle Contract
;; On-chain price feed with multiple reporters
;; Halal - transparent pricing
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-REPORTER (err u402))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var feed-count uint u0)

(define-map reporters principal bool)
(define-map price-feeds (string-ascii 20) { price: uint, last-updated: uint, reporter: principal, decimals: uint })
(define-map price-history { pair: (string-ascii 20), index: uint } { price: uint, block: uint, reporter: principal })
(define-map history-count (string-ascii 20) uint)

(map-set reporters CONTRACT-OWNER true)

(define-public (add-reporter (r principal))
  (begin (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED) (map-set reporters r true) (ok true)))

(define-public (remove-reporter (r principal))
  (begin (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED) (map-set reporters r false) (ok true)))

(define-public (submit-price (pair (string-ascii 20)) (price uint) (decimals uint))
  (let ((idx (default-to u0 (map-get? history-count pair))))
    (asserts! (default-to false (map-get? reporters tx-sender)) ERR-NOT-REPORTER)
    (map-set price-feeds pair { price: price, last-updated: stacks-block-height, reporter: tx-sender, decimals: decimals })
    (map-set price-history { pair: pair, index: idx } { price: price, block: stacks-block-height, reporter: tx-sender })
    (map-set history-count pair (+ idx u1))
    (if (is-none (map-get? price-feeds pair)) (var-set feed-count (+ (var-get feed-count) u1)) true)
    (ok price)))

(define-read-only (get-price (pair (string-ascii 20))) (map-get? price-feeds pair))
(define-read-only (get-price-at (pair (string-ascii 20)) (index uint)) (map-get? price-history { pair: pair, index: index }))
(define-read-only (get-history-length (pair (string-ascii 20))) (ok (default-to u0 (map-get? history-count pair))))
(define-read-only (get-feed-count) (ok (var-get feed-count)))
(define-read-only (is-reporter (who principal)) (ok (default-to false (map-get? reporters who))))
