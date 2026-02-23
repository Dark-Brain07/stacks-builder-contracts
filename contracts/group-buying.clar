;; Group Buying Contract
;; Collective purchasing for better prices
;; Halal - cooperative commerce
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-CLOSED (err u405))
(define-constant ERR-MIN-NOT-MET (err u406))

(define-data-var deal-count uint u0)
(define-data-var total-volume uint u0)

(define-map group-deals uint {
  organizer: principal, product: (string-utf8 100), unit-price: uint,
  min-buyers: uint, max-buyers: uint, current-buyers: uint,
  total-collected: uint, deadline: uint, status: (string-ascii 20)
})
(define-map deal-participants { deal-id: uint, buyer: principal } { quantity: uint, paid: uint })

(define-public (create-deal (product (string-utf8 100)) (unit-price uint) (min-buyers uint) (max-buyers uint) (duration uint))
  (let ((id (+ (var-get deal-count) u1)))
    (map-set group-deals id { organizer: tx-sender, product: product, unit-price: unit-price, min-buyers: min-buyers, max-buyers: max-buyers, current-buyers: u0, total-collected: u0, deadline: (+ stacks-block-height duration), status: "open" })
    (var-set deal-count id) (ok id)))

(define-public (join-deal (deal-id uint) (quantity uint))
  (let (
    (deal (unwrap! (map-get? group-deals deal-id) ERR-NOT-FOUND))
    (cost (* quantity (get unit-price deal)))
  )
    (asserts! (is-eq (get status deal) "open") ERR-CLOSED)
    (asserts! (< (get current-buyers deal) (get max-buyers deal)) ERR-CLOSED)
    (try! (stx-transfer? cost tx-sender (get organizer deal)))
    (map-set deal-participants { deal-id: deal-id, buyer: tx-sender } { quantity: quantity, paid: cost })
    (map-set group-deals deal-id (merge deal { current-buyers: (+ (get current-buyers deal) u1), total-collected: (+ (get total-collected deal) cost) }))
    (var-set total-volume (+ (var-get total-volume) cost))
    (ok cost)))

(define-public (finalize-deal (deal-id uint))
  (let ((deal (unwrap! (map-get? group-deals deal-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer deal)) ERR-NOT-AUTHORIZED)
    (asserts! (>= (get current-buyers deal) (get min-buyers deal)) ERR-MIN-NOT-MET)
    (map-set group-deals deal-id (merge deal { status: "finalized" })) (ok true)))

(define-public (cancel-deal (deal-id uint))
  (let ((deal (unwrap! (map-get? group-deals deal-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer deal)) ERR-NOT-AUTHORIZED)
    (map-set group-deals deal-id (merge deal { status: "cancelled" })) (ok true)))

(define-public (refund-buyer (deal-id uint) (buyer principal))
  (let (
    (deal (unwrap! (map-get? group-deals deal-id) ERR-NOT-FOUND))
    (participant (unwrap! (map-get? deal-participants { deal-id: deal-id, buyer: buyer }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get organizer deal)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? (get paid participant) tx-sender buyer))
    (ok (get paid participant))))

(define-read-only (get-deal (id uint)) (map-get? group-deals id))
(define-read-only (get-participant (deal-id uint) (buyer principal)) (map-get? deal-participants { deal-id: deal-id, buyer: buyer }))
(define-read-only (get-deal-count) (ok (var-get deal-count)))
(define-read-only (get-total-volume) (ok (var-get total-volume)))
