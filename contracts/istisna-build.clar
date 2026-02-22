;; Istisna Build Contract
;; Islamic manufacturing/construction contract
;; Halal - istisna (commission to manufacture)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-COMPLETE (err u405))

(define-data-var order-count uint u0)
(define-data-var total-value uint u0)

(define-map orders uint {
  buyer: principal, builder: principal, specification: (string-utf8 200),
  total-price: uint, paid: uint, milestones: uint, milestones-complete: uint,
  status: (string-ascii 20), created: uint
})
(define-map build-milestones { order-id: uint, index: uint } { description: (string-utf8 100), payment: uint, completed: bool, approved: bool })

(define-public (create-order (builder principal) (spec (string-utf8 200)) (total-price uint) (down-payment uint))
  (let ((id (+ (var-get order-count) u1)))
    (try! (stx-transfer? down-payment tx-sender CONTRACT-OWNER))
    (map-set orders id {
      buyer: tx-sender, builder: builder, specification: spec,
      total-price: total-price, paid: down-payment, milestones: u0, milestones-complete: u0,
      status: "active", created: stacks-block-height
    })
    (var-set order-count id)
    (var-set total-value (+ (var-get total-value) total-price))
    (ok id)))

(define-public (add-milestone (order-id uint) (description (string-utf8 100)) (payment uint))
  (let (
    (order (unwrap! (map-get? orders order-id) ERR-NOT-FOUND))
    (idx (get milestones order))
  )
    (asserts! (or (is-eq tx-sender (get buyer order)) (is-eq tx-sender CONTRACT-OWNER)) ERR-NOT-AUTHORIZED)
    (map-set build-milestones { order-id: order-id, index: idx } { description: description, payment: payment, completed: false, approved: false })
    (map-set orders order-id (merge order { milestones: (+ idx u1) }))
    (ok idx)))

(define-public (complete-milestone (order-id uint) (milestone-index uint))
  (let (
    (order (unwrap! (map-get? orders order-id) ERR-NOT-FOUND))
    (ms (unwrap! (map-get? build-milestones { order-id: order-id, index: milestone-index }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get builder order)) ERR-NOT-AUTHORIZED)
    (map-set build-milestones { order-id: order-id, index: milestone-index } (merge ms { completed: true }))
    (ok true)))

(define-public (approve-and-pay (order-id uint) (milestone-index uint))
  (let (
    (order (unwrap! (map-get? orders order-id) ERR-NOT-FOUND))
    (ms (unwrap! (map-get? build-milestones { order-id: order-id, index: milestone-index }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get buyer order)) ERR-NOT-AUTHORIZED)
    (asserts! (get completed ms) ERR-NOT-FOUND)
    (asserts! (not (get approved ms)) ERR-ALREADY-COMPLETE)
    (try! (stx-transfer? (get payment ms) CONTRACT-OWNER (get builder order)))
    (map-set build-milestones { order-id: order-id, index: milestone-index } (merge ms { approved: true }))
    (map-set orders order-id (merge order { paid: (+ (get paid order) (get payment ms)), milestones-complete: (+ (get milestones-complete order) u1) }))
    (ok (get payment ms))))

(define-public (finalize-order (order-id uint))
  (let ((order (unwrap! (map-get? orders order-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get buyer order)) ERR-NOT-AUTHORIZED)
    (map-set orders order-id (merge order { status: "completed" })) (ok true)))

(define-read-only (get-order (id uint)) (map-get? orders id))
(define-read-only (get-milestone (order-id uint) (index uint)) (map-get? build-milestones { order-id: order-id, index: index }))
(define-read-only (get-order-count) (ok (var-get order-count)))
(define-read-only (get-total-value) (ok (var-get total-value)))
