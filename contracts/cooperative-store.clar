;; Cooperative Store Contract
;; Community-owned cooperative marketplace
;; Halal - cooperative commerce
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-NOT-MEMBER (err u405))
(define-constant ERR-OUT-OF-STOCK (err u406))

(define-data-var member-count uint u0)
(define-data-var product-count uint u0)
(define-data-var total-sales uint u0)

(define-map members principal { share: uint, joined: uint, purchases: uint, dividends-claimed: uint })
(define-map products uint { name: (string-utf8 100), price: uint, stock: uint, sold: uint, supplier: principal })
(define-data-var total-revenue uint u0)
(define-data-var total-dividends uint u0)

(define-public (join-coop (share-amount uint))
  (begin
    (asserts! (is-none (map-get? members tx-sender)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? share-amount tx-sender CONTRACT-OWNER))
    (map-set members tx-sender { share: share-amount, joined: stacks-block-height, purchases: u0, dividends-claimed: u0 })
    (var-set member-count (+ (var-get member-count) u1))
    (ok true)))

(define-public (add-product (name (string-utf8 100)) (price uint) (stock uint))
  (let ((id (+ (var-get product-count) u1)))
    (asserts! (is-some (map-get? members tx-sender)) ERR-NOT-MEMBER)
    (map-set products id { name: name, price: price, stock: stock, sold: u0, supplier: tx-sender })
    (var-set product-count id) (ok id)))

(define-public (purchase (product-id uint) (quantity uint))
  (let (
    (product (unwrap! (map-get? products product-id) ERR-NOT-FOUND))
    (cost (* (get price product) quantity))
    (member (unwrap! (map-get? members tx-sender) ERR-NOT-MEMBER))
  )
    (asserts! (>= (get stock product) quantity) ERR-OUT-OF-STOCK)
    (try! (stx-transfer? cost tx-sender (get supplier product)))
    (map-set products product-id (merge product { stock: (- (get stock product) quantity), sold: (+ (get sold product) quantity) }))
    (map-set members tx-sender (merge member { purchases: (+ (get purchases member) u1) }))
    (var-set total-sales (+ (var-get total-sales) cost))
    (var-set total-revenue (+ (var-get total-revenue) cost))
    (ok cost)))

(define-public (distribute-dividend (member principal) (amount uint))
  (let ((m (unwrap! (map-get? members member) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? amount tx-sender member))
    (map-set members member (merge m { dividends-claimed: (+ (get dividends-claimed m) amount) }))
    (var-set total-dividends (+ (var-get total-dividends) amount))
    (ok amount)))

(define-public (restock (product-id uint) (quantity uint))
  (let ((product (unwrap! (map-get? products product-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get supplier product)) ERR-NOT-AUTHORIZED)
    (map-set products product-id (merge product { stock: (+ (get stock product) quantity) })) (ok true)))

(define-read-only (get-member (who principal)) (map-get? members who))
(define-read-only (get-product (id uint)) (map-get? products id))
(define-read-only (get-member-count) (ok (var-get member-count)))
(define-read-only (get-total-sales) (ok (var-get total-sales)))
(define-read-only (get-product-count) (ok (var-get product-count)))
