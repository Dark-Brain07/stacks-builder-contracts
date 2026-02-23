;; Salam Forward Contract
;; Islamic forward sale - pay now, deliver later
;; Halal - salam (advance payment for future goods)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-DELIVERED (err u405))
(define-constant ERR-NOT-DUE (err u406))

(define-data-var contract-count uint u0)
(define-data-var total-volume uint u0)

(define-map salam-contracts uint {
  buyer: principal, seller: principal, commodity: (string-utf8 100),
  quantity: uint, price: uint, delivery-block: uint,
  delivered: bool, status: (string-ascii 20), created: uint
})
(define-map seller-contracts principal uint)

(define-public (create-salam (seller principal) (commodity (string-utf8 100)) (quantity uint) (price uint) (delivery-blocks uint))
  (let ((id (+ (var-get contract-count) u1)))
    (try! (stx-transfer? price tx-sender seller))
    (map-set salam-contracts id {
      buyer: tx-sender, seller: seller, commodity: commodity,
      quantity: quantity, price: price, delivery-block: (+ stacks-block-height delivery-blocks),
      delivered: false, status: "active", created: stacks-block-height
    })
    (map-set seller-contracts seller (+ (default-to u0 (map-get? seller-contracts seller)) u1))
    (var-set contract-count id)
    (var-set total-volume (+ (var-get total-volume) price))
    (print { event: "salam-created", id: id, buyer: tx-sender, seller: seller, commodity: commodity })
    (ok id)))

(define-public (confirm-delivery (contract-id uint))
  (let ((sc (unwrap! (map-get? salam-contracts contract-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get buyer sc)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get delivered sc)) ERR-ALREADY-DELIVERED)
    (map-set salam-contracts contract-id (merge sc { delivered: true, status: "completed" }))
    (print { event: "salam-delivered", id: contract-id })
    (ok true)))

(define-public (dispute-contract (contract-id uint))
  (let ((sc (unwrap! (map-get? salam-contracts contract-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get buyer sc)) ERR-NOT-AUTHORIZED)
    (asserts! (> stacks-block-height (get delivery-block sc)) ERR-NOT-DUE)
    (asserts! (not (get delivered sc)) ERR-ALREADY-DELIVERED)
    (map-set salam-contracts contract-id (merge sc { status: "disputed" }))
    (ok true)))

(define-public (resolve-dispute (contract-id uint) (refund bool))
  (let ((sc (unwrap! (map-get? salam-contracts contract-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (if refund
      (begin (try! (stx-transfer? (get price sc) (get seller sc) (get buyer sc)))
        (map-set salam-contracts contract-id (merge sc { status: "refunded" })))
      (map-set salam-contracts contract-id (merge sc { status: "resolved" })))
    (ok true)))

(define-read-only (get-contract (id uint)) (map-get? salam-contracts id))
(define-read-only (get-contract-count) (ok (var-get contract-count)))
(define-read-only (get-total-volume) (ok (var-get total-volume)))
(define-read-only (is-overdue (id uint))
  (match (map-get? salam-contracts id) sc (ok (and (not (get delivered sc)) (> stacks-block-height (get delivery-block sc)))) (ok false)))
