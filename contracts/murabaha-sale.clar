;; Murabaha Sale Contract
;; Islamic cost-plus sale financing
;; Halal - murabaha (transparent markup, no interest)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-PAID (err u405))

(define-data-var sale-count uint u0)
(define-data-var total-sales uint u0)

(define-map murabaha-sales uint {
  seller: principal, buyer: principal, item: (string-utf8 100),
  cost-price: uint, markup-pct: uint, sale-price: uint,
  paid: uint, installments: uint, installments-paid: uint,
  status: (string-ascii 20), created: uint
})
(define-map installment-payments { sale-id: uint, index: uint } { amount: uint, block: uint })

(define-public (create-sale (buyer principal) (item (string-utf8 100)) (cost-price uint) (markup-pct uint) (installments uint))
  (let (
    (id (+ (var-get sale-count) u1))
    (markup (/ (* cost-price markup-pct) u100))
    (sale-price (+ cost-price markup))
  )
    (map-set murabaha-sales id {
      seller: tx-sender, buyer: buyer, item: item,
      cost-price: cost-price, markup-pct: markup-pct, sale-price: sale-price,
      paid: u0, installments: installments, installments-paid: u0,
      status: "active", created: stacks-block-height
    })
    (var-set sale-count id)
    (var-set total-sales (+ (var-get total-sales) sale-price))
    (print { event: "murabaha-created", id: id, cost: cost-price, markup: markup, total: sale-price })
    (ok id)))

(define-public (pay-installment (sale-id uint))
  (let (
    (sale (unwrap! (map-get? murabaha-sales sale-id) ERR-NOT-FOUND))
    (installment-amount (/ (get sale-price sale) (get installments sale)))
    (new-paid (+ (get installments-paid sale) u1))
  )
    (asserts! (is-eq tx-sender (get buyer sale)) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-eq (get status sale) "completed")) ERR-ALREADY-PAID)
    (try! (stx-transfer? installment-amount tx-sender (get seller sale)))
    (map-set installment-payments { sale-id: sale-id, index: (get installments-paid sale) } { amount: installment-amount, block: stacks-block-height })
    (map-set murabaha-sales sale-id (merge sale {
      paid: (+ (get paid sale) installment-amount),
      installments-paid: new-paid,
      status: (if (>= new-paid (get installments sale)) "completed" "active")
    }))
    (ok installment-amount)))

(define-public (pay-full (sale-id uint))
  (let (
    (sale (unwrap! (map-get? murabaha-sales sale-id) ERR-NOT-FOUND))
    (remaining (- (get sale-price sale) (get paid sale)))
  )
    (asserts! (is-eq tx-sender (get buyer sale)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? remaining tx-sender (get seller sale)))
    (map-set murabaha-sales sale-id (merge sale { paid: (get sale-price sale), installments-paid: (get installments sale), status: "completed" }))
    (ok remaining)))

(define-read-only (get-sale (id uint)) (map-get? murabaha-sales id))
(define-read-only (get-payment (sale-id uint) (index uint)) (map-get? installment-payments { sale-id: sale-id, index: index }))
(define-read-only (get-sale-count) (ok (var-get sale-count)))
(define-read-only (get-total-sales) (ok (var-get total-sales)))
(define-read-only (get-remaining (id uint))
  (match (map-get? murabaha-sales id) s (ok (- (get sale-price s) (get paid s))) (ok u0)))
