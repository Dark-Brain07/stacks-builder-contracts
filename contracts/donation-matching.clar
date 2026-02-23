;; Donation Matching Contract
;; Match charitable donations from a pool
;; Halal - amplified charity
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-POOL-EMPTY (err u405))
(define-constant ERR-ENDED (err u406))

(define-data-var match-pool uint u0)
(define-data-var campaign-count uint u0)
(define-data-var total-matched uint u0)

(define-map campaigns uint {
  name: (string-utf8 100), recipient: principal, match-ratio: uint,
  max-match: uint, matched-so-far: uint, deadline: uint, active: bool
})
(define-map donations { campaign-id: uint, donor: principal } { donated: uint, matched: uint })

(define-public (fund-match-pool (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (var-set match-pool (+ (var-get match-pool) amount))
    (ok amount)))

(define-public (create-campaign (name (string-utf8 100)) (recipient principal) (ratio uint) (max-match uint) (duration uint))
  (let ((id (+ (var-get campaign-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set campaigns id { name: name, recipient: recipient, match-ratio: ratio, max-match: max-match, matched-so-far: u0, deadline: (+ stacks-block-height duration), active: true })
    (var-set campaign-count id) (ok id)))

(define-public (donate-and-match (campaign-id uint) (amount uint))
  (let (
    (campaign (unwrap! (map-get? campaigns campaign-id) ERR-NOT-FOUND))
    (match-amount (/ (* amount (get match-ratio campaign)) u100))
    (capped-match (if (> (+ (get matched-so-far campaign) match-amount) (get max-match campaign))
      (- (get max-match campaign) (get matched-so-far campaign)) match-amount))
    (prev (default-to { donated: u0, matched: u0 } (map-get? donations { campaign-id: campaign-id, donor: tx-sender })))
  )
    (asserts! (get active campaign) ERR-ENDED)
    (asserts! (< stacks-block-height (get deadline campaign)) ERR-ENDED)
    (try! (stx-transfer? amount tx-sender (get recipient campaign)))
    (if (and (> capped-match u0) (>= (var-get match-pool) capped-match))
      (begin
        (try! (stx-transfer? capped-match CONTRACT-OWNER (get recipient campaign)))
        (var-set match-pool (- (var-get match-pool) capped-match))
        (var-set total-matched (+ (var-get total-matched) capped-match))
        (map-set campaigns campaign-id (merge campaign { matched-so-far: (+ (get matched-so-far campaign) capped-match) }))
        (map-set donations { campaign-id: campaign-id, donor: tx-sender } { donated: (+ (get donated prev) amount), matched: (+ (get matched prev) capped-match) })
        (ok (+ amount capped-match)))
      (begin
        (map-set donations { campaign-id: campaign-id, donor: tx-sender } { donated: (+ (get donated prev) amount), matched: (get matched prev) })
        (ok amount)))))

(define-read-only (get-campaign (id uint)) (map-get? campaigns id))
(define-read-only (get-donation (campaign-id uint) (donor principal)) (map-get? donations { campaign-id: campaign-id, donor: donor }))
(define-read-only (get-match-pool) (ok (var-get match-pool)))
(define-read-only (get-total-matched) (ok (var-get total-matched)))
(define-read-only (get-campaign-count) (ok (var-get campaign-count)))
