;; Musharakah Contract (Islamic Partnership)
;; Halal profit/loss sharing partnership
;; Based on equity contribution ratios
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-PARTNER (err u405))
(define-constant ERR-NOT-PARTNER (err u406))

(define-data-var partnership-count uint u0)

(define-map partnerships uint {
  name: (string-utf8 100), total-capital: uint, total-profit: uint,
  partner-count: uint, status: (string-ascii 20), created: uint
})
(define-map partners { partnership-id: uint, partner: principal } { capital: uint, profit-share: uint, joined: uint })
(define-map partner-withdrawals { partnership-id: uint, partner: principal } uint)

(define-public (create-partnership (name (string-utf8 100)) (initial-capital uint))
  (let ((id (+ (var-get partnership-count) u1)))
    (try! (stx-transfer? initial-capital tx-sender CONTRACT-OWNER))
    (map-set partnerships id { name: name, total-capital: initial-capital, total-profit: u0, partner-count: u1, status: "active", created: stacks-block-height })
    (map-set partners { partnership-id: id, partner: tx-sender } { capital: initial-capital, profit-share: u0, joined: stacks-block-height })
    (var-set partnership-count id) (ok id)))

(define-public (join-partnership (partnership-id uint) (capital uint))
  (let ((p (unwrap! (map-get? partnerships partnership-id) ERR-NOT-FOUND)))
    (asserts! (is-none (map-get? partners { partnership-id: partnership-id, partner: tx-sender })) ERR-ALREADY-PARTNER)
    (try! (stx-transfer? capital tx-sender CONTRACT-OWNER))
    (map-set partners { partnership-id: partnership-id, partner: tx-sender } { capital: capital, profit-share: u0, joined: stacks-block-height })
    (map-set partnerships partnership-id (merge p { total-capital: (+ (get total-capital p) capital), partner-count: (+ (get partner-count p) u1) }))
    (ok true)))

(define-public (add-profit (partnership-id uint) (amount uint))
  (let ((p (unwrap! (map-get? partnerships partnership-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set partnerships partnership-id (merge p { total-profit: (+ (get total-profit p) amount) }))
    (ok true)))

(define-public (distribute-to-partner (partnership-id uint) (partner principal) (amount uint))
  (let (
    (p (unwrap! (map-get? partnerships partnership-id) ERR-NOT-FOUND))
    (prt (unwrap! (map-get? partners { partnership-id: partnership-id, partner: partner }) ERR-NOT-PARTNER))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? amount tx-sender partner))
    (map-set partners { partnership-id: partnership-id, partner: partner } (merge prt { profit-share: (+ (get profit-share prt) amount) }))
    (ok amount)))

(define-public (close-partnership (partnership-id uint))
  (let ((p (unwrap! (map-get? partnerships partnership-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set partnerships partnership-id (merge p { status: "closed" })) (ok true)))

(define-read-only (get-partnership (id uint)) (map-get? partnerships id))
(define-read-only (get-partner (partnership-id uint) (partner principal)) (map-get? partners { partnership-id: partnership-id, partner: partner }))
(define-read-only (get-partnership-count) (ok (var-get partnership-count)))
(define-read-only (get-capital-share (partnership-id uint) (partner principal))
  (match (map-get? partners { partnership-id: partnership-id, partner: partner })
    prt (match (map-get? partnerships partnership-id) p (ok (/ (* (get capital prt) u100) (get total-capital p))) (ok u0))
    (ok u0)))
