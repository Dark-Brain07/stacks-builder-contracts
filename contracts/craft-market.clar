;; Craft Market Contract
;; Artisan crafts marketplace
;; Halal - honest trade of handcrafts
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-SOLD-OUT (err u405))
(define-constant MARKET-FEE-PCT u3)

(define-data-var product-count uint u0)
(define-data-var order-count uint u0)
(define-data-var total-sales uint u0)

(define-map artisans principal { name: (string-utf8 100), craft-type: (string-utf8 50), products: uint, sales: uint, earned: uint })
(define-map products uint { artisan: principal, name: (string-utf8 100), description: (string-utf8 200), price: uint, stock: uint, sold: uint, active: bool })
(define-map orders uint { product-id: uint, buyer: principal, quantity: uint, total: uint, status: (string-ascii 20), block: uint })

(define-public (register-artisan (name (string-utf8 100)) (craft-type (string-utf8 50)))
  (begin (map-set artisans tx-sender { name: name, craft-type: craft-type, products: u0, sales: u0, earned: u0 }) (ok true)))

(define-public (list-product (name (string-utf8 100)) (description (string-utf8 200)) (price uint) (stock uint))
  (let (
    (id (+ (var-get product-count) u1))
    (a (unwrap! (map-get? artisans tx-sender) ERR-NOT-AUTHORIZED))
  )
    (map-set products id { artisan: tx-sender, name: name, description: description, price: price, stock: stock, sold: u0, active: true })
    (map-set artisans tx-sender (merge a { products: (+ (get products a) u1) }))
    (var-set product-count id) (ok id)))

(define-public (buy-product (product-id uint) (quantity uint))
  (let (
    (p (unwrap! (map-get? products product-id) ERR-NOT-FOUND))
    (total (* quantity (get price p)))
    (fee (/ (* total MARKET-FEE-PCT) u100))
    (artisan-pay (- total fee))
    (oid (+ (var-get order-count) u1))
    (a (unwrap! (map-get? artisans (get artisan p)) ERR-NOT-FOUND))
  )
    (asserts! (get active p) ERR-NOT-FOUND)
    (asserts! (>= (get stock p) quantity) ERR-SOLD-OUT)
    (try! (stx-transfer? artisan-pay tx-sender (get artisan p)))
    (try! (stx-transfer? fee tx-sender CONTRACT-OWNER))
    (map-set orders oid { product-id: product-id, buyer: tx-sender, quantity: quantity, total: total, status: "ordered", block: stacks-block-height })
    (map-set products product-id (merge p { stock: (- (get stock p) quantity), sold: (+ (get sold p) quantity) }))
    (map-set artisans (get artisan p) (merge a { sales: (+ (get sales a) u1), earned: (+ (get earned a) artisan-pay) }))
    (var-set order-count oid)
    (var-set total-sales (+ (var-get total-sales) total)) (ok oid)))

(define-public (fulfill-order (order-id uint))
  (let (
    (o (unwrap! (map-get? orders order-id) ERR-NOT-FOUND))
    (p (unwrap! (map-get? products (get product-id o)) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get artisan p)) ERR-NOT-AUTHORIZED)
    (map-set orders order-id (merge o { status: "shipped" })) (ok true)))

(define-read-only (get-artisan (who principal)) (map-get? artisans who))
(define-read-only (get-product (id uint)) (map-get? products id))
(define-read-only (get-order (id uint)) (map-get? orders id))
(define-read-only (get-product-count) (ok (var-get product-count)))
(define-read-only (get-total-sales) (ok (var-get total-sales)))
