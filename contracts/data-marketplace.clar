;; Data Marketplace Contract
;; Buy and sell datasets on-chain
;; Halal - knowledge commerce
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-PURCHASED (err u405))
(define-constant PLATFORM-FEE-PCT u3) ;; 3% platform fee

(define-data-var dataset-count uint u0)
(define-data-var total-sales uint u0)

(define-map datasets uint {
  seller: principal, title: (string-utf8 100), description: (string-utf8 200),
  price: uint, data-hash: (buff 32), sales: uint, active: bool, listed: uint
})
(define-map purchases { dataset-id: uint, buyer: principal } { purchased-at: uint, price-paid: uint })
(define-map seller-revenue principal uint)

(define-public (list-dataset (title (string-utf8 100)) (description (string-utf8 200)) (price uint) (data-hash (buff 32)))
  (let ((id (+ (var-get dataset-count) u1)))
    (map-set datasets id { seller: tx-sender, title: title, description: description, price: price, data-hash: data-hash, sales: u0, active: true, listed: stacks-block-height })
    (var-set dataset-count id) (ok id)))

(define-public (buy-dataset (dataset-id uint))
  (let (
    (ds (unwrap! (map-get? datasets dataset-id) ERR-NOT-FOUND))
    (fee (/ (* (get price ds) PLATFORM-FEE-PCT) u100))
    (seller-amount (- (get price ds) fee))
    (prev-rev (default-to u0 (map-get? seller-revenue (get seller ds))))
  )
    (asserts! (get active ds) ERR-NOT-FOUND)
    (asserts! (is-none (map-get? purchases { dataset-id: dataset-id, buyer: tx-sender })) ERR-ALREADY-PURCHASED)
    (try! (stx-transfer? seller-amount tx-sender (get seller ds)))
    (try! (stx-transfer? fee tx-sender CONTRACT-OWNER))
    (map-set purchases { dataset-id: dataset-id, buyer: tx-sender } { purchased-at: stacks-block-height, price-paid: (get price ds) })
    (map-set datasets dataset-id (merge ds { sales: (+ (get sales ds) u1) }))
    (map-set seller-revenue (get seller ds) (+ prev-rev seller-amount))
    (var-set total-sales (+ (var-get total-sales) u1))
    (ok true)))

(define-public (delist-dataset (dataset-id uint))
  (let ((ds (unwrap! (map-get? datasets dataset-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get seller ds)) ERR-NOT-AUTHORIZED)
    (map-set datasets dataset-id (merge ds { active: false })) (ok true)))

(define-public (update-price (dataset-id uint) (new-price uint))
  (let ((ds (unwrap! (map-get? datasets dataset-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get seller ds)) ERR-NOT-AUTHORIZED)
    (map-set datasets dataset-id (merge ds { price: new-price })) (ok true)))

(define-read-only (get-dataset (id uint)) (map-get? datasets id))
(define-read-only (get-purchase (dataset-id uint) (buyer principal)) (map-get? purchases { dataset-id: dataset-id, buyer: buyer }))
(define-read-only (get-dataset-count) (ok (var-get dataset-count)))
(define-read-only (get-total-sales) (ok (var-get total-sales)))
(define-read-only (get-seller-revenue (seller principal)) (ok (default-to u0 (map-get? seller-revenue seller))))
(define-read-only (has-purchased (dataset-id uint) (buyer principal)) (ok (is-some (map-get? purchases { dataset-id: dataset-id, buyer: buyer }))))
