;; Prayer Circle Contract
;; Community prayer request and dua sharing
;; Halal - communal prayer (dua)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var request-count uint u0)
(define-data-var total-prayers uint u0)

(define-map prayer-requests uint {
  requester: principal, intention: (string-utf8 200), category: (string-ascii 20),
  prayers-received: uint, is-anonymous: bool, status: (string-ascii 20), posted: uint
})
(define-map prayer-responses { request-id: uint, respondent: principal } { message: (string-utf8 200), block: uint })
(define-map member-prayer-stats principal { requests-made: uint, prayers-offered: uint })

(define-public (submit-request (intention (string-utf8 200)) (category (string-ascii 20)) (anonymous bool))
  (let (
    (id (+ (var-get request-count) u1))
    (stats (default-to { requests-made: u0, prayers-offered: u0 } (map-get? member-prayer-stats tx-sender)))
  )
    (map-set prayer-requests id { requester: tx-sender, intention: intention, category: category, prayers-received: u0, is-anonymous: anonymous, status: "active", posted: stacks-block-height })
    (map-set member-prayer-stats tx-sender (merge stats { requests-made: (+ (get requests-made stats) u1) }))
    (var-set request-count id) (ok id)))

(define-public (offer-prayer (request-id uint) (message (string-utf8 200)))
  (let (
    (req (unwrap! (map-get? prayer-requests request-id) ERR-NOT-FOUND))
    (stats (default-to { requests-made: u0, prayers-offered: u0 } (map-get? member-prayer-stats tx-sender)))
  )
    (map-set prayer-responses { request-id: request-id, respondent: tx-sender } { message: message, block: stacks-block-height })
    (map-set prayer-requests request-id (merge req { prayers-received: (+ (get prayers-received req) u1) }))
    (map-set member-prayer-stats tx-sender (merge stats { prayers-offered: (+ (get prayers-offered stats) u1) }))
    (var-set total-prayers (+ (var-get total-prayers) u1)) (ok true)))

(define-public (mark-answered (request-id uint))
  (let ((req (unwrap! (map-get? prayer-requests request-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get requester req)) ERR-NOT-AUTHORIZED)
    (map-set prayer-requests request-id (merge req { status: "answered" }))
    (print { event: "prayer-answered", request: request-id }) (ok true)))

(define-public (close-request (request-id uint))
  (let ((req (unwrap! (map-get? prayer-requests request-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get requester req)) ERR-NOT-AUTHORIZED)
    (map-set prayer-requests request-id (merge req { status: "closed" })) (ok true)))

(define-read-only (get-request (id uint)) (map-get? prayer-requests id))
(define-read-only (get-response (request-id uint) (who principal)) (map-get? prayer-responses { request-id: request-id, respondent: who }))
(define-read-only (get-stats (who principal)) (map-get? member-prayer-stats who))
(define-read-only (get-request-count) (ok (var-get request-count)))
(define-read-only (get-total-prayers) (ok (var-get total-prayers)))
