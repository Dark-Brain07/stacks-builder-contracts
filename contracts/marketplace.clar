;; Marketplace Contract
;; Peer-to-peer marketplace for goods and services
;; Halal - legitimate trade (tijara)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-LISTING-NOT-FOUND (err u404))
(define-constant ERR-LISTING-SOLD (err u405))
(define-constant ERR-SELF-BUY (err u406))

(define-data-var listing-count uint u0)
(define-data-var total-sales uint u0)

(define-map listings uint {
  seller: principal,
  title: (string-utf8 100),
  price: uint,
  sold: bool,
  buyer: (optional principal),
  created-at: uint
})
(define-map seller-listings principal (list 20 uint))
(define-map seller-revenue principal uint)

(define-public (create-listing (title (string-utf8 100)) (price uint))
  (let ((id (+ (var-get listing-count) u1)))
    (map-set listings id {
      seller: tx-sender, title: title, price: price,
      sold: false, buyer: none, created-at: stacks-block-height
    })
    (var-set listing-count id)
    (ok id)))

(define-public (buy-listing (id uint))
  (let ((listing (unwrap! (map-get? listings id) ERR-LISTING-NOT-FOUND))
        (prev-rev (default-to u0 (map-get? seller-revenue (get seller listing)))))
    (asserts! (not (get sold listing)) ERR-LISTING-SOLD)
    (asserts! (not (is-eq tx-sender (get seller listing))) ERR-SELF-BUY)
    (try! (stx-transfer? (get price listing) tx-sender (get seller listing)))
    (map-set listings id (merge listing { sold: true, buyer: (some tx-sender) }))
    (map-set seller-revenue (get seller listing) (+ prev-rev (get price listing)))
    (var-set total-sales (+ (var-get total-sales) u1))
    (print { event: "sale", id: id, buyer: tx-sender, price: (get price listing) })
    (ok true)))

(define-public (cancel-listing (id uint))
  (let ((listing (unwrap! (map-get? listings id) ERR-LISTING-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get seller listing)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get sold listing)) ERR-LISTING-SOLD)
    (map-set listings id (merge listing { sold: true, buyer: none }))
    (ok true)))

(define-public (update-price (id uint) (new-price uint))
  (let ((listing (unwrap! (map-get? listings id) ERR-LISTING-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get seller listing)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get sold listing)) ERR-LISTING-SOLD)
    (map-set listings id (merge listing { price: new-price }))
    (ok true)))

(define-read-only (get-listing (id uint)) (map-get? listings id))
(define-read-only (get-listing-count) (ok (var-get listing-count)))
(define-read-only (get-total-sales) (ok (var-get total-sales)))
(define-read-only (get-seller-revenue (seller principal))
  (ok (default-to u0 (map-get? seller-revenue seller))))
