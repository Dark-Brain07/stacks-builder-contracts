;; Revenue Share Contract
;; Automated revenue sharing among partners
;; Halal - fair partnership distribution
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-OVER-100 (err u405))

(define-data-var agreement-count uint u0)
(define-data-var total-distributed uint u0)

(define-map agreements uint { creator: principal, name: (string-utf8 100), partner-count: uint, total-revenue: uint, active: bool, created: uint })
(define-map partners { agreement-id: uint, index: uint } { partner: principal, share-pct: uint, earned: uint })
(define-map partner-lookup { agreement-id: uint, partner: principal } uint)

(define-public (create-agreement (name (string-utf8 100)))
  (let ((id (+ (var-get agreement-count) u1)))
    (map-set agreements id { creator: tx-sender, name: name, partner-count: u0, total-revenue: u0, active: true, created: stacks-block-height })
    (var-set agreement-count id) (ok id)))

(define-public (add-partner (agreement-id uint) (partner principal) (share-pct uint))
  (let (
    (agr (unwrap! (map-get? agreements agreement-id) ERR-NOT-FOUND))
    (idx (get partner-count agr))
  )
    (asserts! (is-eq tx-sender (get creator agr)) ERR-NOT-AUTHORIZED)
    (map-set partners { agreement-id: agreement-id, index: idx } { partner: partner, share-pct: share-pct, earned: u0 })
    (map-set partner-lookup { agreement-id: agreement-id, partner: partner } idx)
    (map-set agreements agreement-id (merge agr { partner-count: (+ idx u1) }))
    (ok idx)))

(define-public (deposit-revenue (agreement-id uint) (amount uint))
  (let ((agr (unwrap! (map-get? agreements agreement-id) ERR-NOT-FOUND)))
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set agreements agreement-id (merge agr { total-revenue: (+ (get total-revenue agr) amount) }))
    (ok amount)))

(define-public (distribute-to-partner (agreement-id uint) (partner-index uint))
  (let (
    (agr (unwrap! (map-get? agreements agreement-id) ERR-NOT-FOUND))
    (p (unwrap! (map-get? partners { agreement-id: agreement-id, index: partner-index }) ERR-NOT-FOUND))
    (share-amount (/ (* (get total-revenue agr) (get share-pct p)) u100))
    (distributable (- share-amount (get earned p)))
  )
    (asserts! (is-eq tx-sender (get creator agr)) ERR-NOT-AUTHORIZED)
    (asserts! (> distributable u0) ERR-NOT-FOUND)
    (try! (stx-transfer? distributable CONTRACT-OWNER (get partner p)))
    (map-set partners { agreement-id: agreement-id, index: partner-index } (merge p { earned: (+ (get earned p) distributable) }))
    (var-set total-distributed (+ (var-get total-distributed) distributable))
    (ok distributable)))

(define-public (deactivate (agreement-id uint))
  (let ((agr (unwrap! (map-get? agreements agreement-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get creator agr)) ERR-NOT-AUTHORIZED)
    (map-set agreements agreement-id (merge agr { active: false })) (ok true)))

(define-read-only (get-agreement (id uint)) (map-get? agreements id))
(define-read-only (get-partner (agreement-id uint) (index uint)) (map-get? partners { agreement-id: agreement-id, index: index }))
(define-read-only (get-partner-index (agreement-id uint) (partner principal)) (map-get? partner-lookup { agreement-id: agreement-id, partner: partner }))
(define-read-only (get-agreement-count) (ok (var-get agreement-count)))
(define-read-only (get-total-distributed) (ok (var-get total-distributed)))
