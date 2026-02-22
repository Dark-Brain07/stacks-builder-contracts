;; Skill Exchange Contract
;; Barter skills directly without money
;; Halal - fair exchange of services
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-COMPLETED (err u405))

(define-data-var listing-count uint u0)
(define-data-var exchange-count uint u0)

(define-map skill-listings uint { provider: principal, offering: (string-utf8 100), seeking: (string-utf8 100), active: bool, posted: uint })
(define-map exchanges uint { listing-id: uint, provider: principal, requester: principal, provider-done: bool, requester-done: bool, status: (string-ascii 20), created: uint })
(define-map user-stats principal { listings: uint, exchanges: uint, rating-total: uint, ratings: uint })

(define-public (list-skill (offering (string-utf8 100)) (seeking (string-utf8 100)))
  (let (
    (id (+ (var-get listing-count) u1))
    (stats (default-to { listings: u0, exchanges: u0, rating-total: u0, ratings: u0 } (map-get? user-stats tx-sender)))
  )
    (map-set skill-listings id { provider: tx-sender, offering: offering, seeking: seeking, active: true, posted: stacks-block-height })
    (map-set user-stats tx-sender (merge stats { listings: (+ (get listings stats) u1) }))
    (var-set listing-count id) (ok id)))

(define-public (request-exchange (listing-id uint))
  (let (
    (listing (unwrap! (map-get? skill-listings listing-id) ERR-NOT-FOUND))
    (eid (+ (var-get exchange-count) u1))
  )
    (asserts! (get active listing) ERR-NOT-FOUND)
    (map-set exchanges eid { listing-id: listing-id, provider: (get provider listing), requester: tx-sender, provider-done: false, requester-done: false, status: "active", created: stacks-block-height })
    (map-set skill-listings listing-id (merge listing { active: false }))
    (var-set exchange-count eid) (ok eid)))

(define-public (mark-done (exchange-id uint))
  (let ((ex (unwrap! (map-get? exchanges exchange-id) ERR-NOT-FOUND)))
    (asserts! (is-eq (get status ex) "active") ERR-ALREADY-COMPLETED)
    (if (is-eq tx-sender (get provider ex))
      (let ((updated (merge ex { provider-done: true })))
        (map-set exchanges exchange-id (if (get requester-done updated) (merge updated { status: "completed" }) updated)) (ok true))
      (if (is-eq tx-sender (get requester ex))
        (let ((updated (merge ex { requester-done: true })))
          (map-set exchanges exchange-id (if (get provider-done updated) (merge updated { status: "completed" }) updated)) (ok true))
        ERR-NOT-AUTHORIZED))))

(define-public (rate-exchange (exchange-id uint) (rating uint))
  (let (
    (ex (unwrap! (map-get? exchanges exchange-id) ERR-NOT-FOUND))
    (other (if (is-eq tx-sender (get provider ex)) (get requester ex) (get provider ex)))
    (stats (default-to { listings: u0, exchanges: u0, rating-total: u0, ratings: u0 } (map-get? user-stats other)))
  )
    (asserts! (is-eq (get status ex) "completed") ERR-NOT-FOUND)
    (map-set user-stats other (merge stats { rating-total: (+ (get rating-total stats) rating), ratings: (+ (get ratings stats) u1) }))
    (ok rating)))

(define-read-only (get-listing (id uint)) (map-get? skill-listings id))
(define-read-only (get-exchange (id uint)) (map-get? exchanges id))
(define-read-only (get-user-stats (who principal)) (map-get? user-stats who))
(define-read-only (get-listing-count) (ok (var-get listing-count)))
(define-read-only (get-exchange-count) (ok (var-get exchange-count)))
