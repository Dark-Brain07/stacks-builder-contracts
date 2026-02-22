;; Neighbor Help Contract
;; Neighborhood mutual aid network
;; Halal - good neighborliness (haqq al-jar)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-HELPED (err u405))

(define-data-var request-count uint u0)
(define-data-var helps-completed uint u0)

(define-map help-requests uint {
  requester: principal, category: (string-ascii 20), description: (string-utf8 200),
  urgency: uint, helper: (optional principal), status: (string-ascii 20), posted: uint
})
(define-map neighbor-stats principal { requests-made: uint, helps-given: uint, karma: uint })

(define-public (request-help (category (string-ascii 20)) (description (string-utf8 200)) (urgency uint))
  (let (
    (id (+ (var-get request-count) u1))
    (stats (default-to { requests-made: u0, helps-given: u0, karma: u0 } (map-get? neighbor-stats tx-sender)))
  )
    (map-set help-requests id { requester: tx-sender, category: category, description: description, urgency: urgency, helper: none, status: "open", posted: stacks-block-height })
    (map-set neighbor-stats tx-sender (merge stats { requests-made: (+ (get requests-made stats) u1) }))
    (var-set request-count id) (ok id)))

(define-public (offer-help (request-id uint))
  (let ((req (unwrap! (map-get? help-requests request-id) ERR-NOT-FOUND)))
    (asserts! (is-eq (get status req) "open") ERR-ALREADY-HELPED)
    (map-set help-requests request-id (merge req { helper: (some tx-sender), status: "helping" }))
    (ok true)))

(define-public (confirm-help (request-id uint))
  (let (
    (req (unwrap! (map-get? help-requests request-id) ERR-NOT-FOUND))
    (helper (unwrap! (get helper req) ERR-NOT-FOUND))
    (helper-stats (default-to { requests-made: u0, helps-given: u0, karma: u0 } (map-get? neighbor-stats helper)))
  )
    (asserts! (is-eq tx-sender (get requester req)) ERR-NOT-AUTHORIZED)
    (map-set help-requests request-id (merge req { status: "completed" }))
    (map-set neighbor-stats helper (merge helper-stats { helps-given: (+ (get helps-given helper-stats) u1), karma: (+ (get karma helper-stats) u10) }))
    (var-set helps-completed (+ (var-get helps-completed) u1))
    (ok true)))

(define-public (cancel-request (request-id uint))
  (let ((req (unwrap! (map-get? help-requests request-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get requester req)) ERR-NOT-AUTHORIZED)
    (map-set help-requests request-id (merge req { status: "cancelled" })) (ok true)))

(define-public (tip-helper (request-id uint) (amount uint))
  (let ((req (unwrap! (map-get? help-requests request-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get requester req)) ERR-NOT-AUTHORIZED)
    (match (get helper req) h (begin (try! (stx-transfer? amount tx-sender h)) (ok amount)) ERR-NOT-FOUND)))

(define-read-only (get-request (id uint)) (map-get? help-requests id))
(define-read-only (get-neighbor-stats (who principal)) (map-get? neighbor-stats who))
(define-read-only (get-request-count) (ok (var-get request-count)))
(define-read-only (get-helps-completed) (ok (var-get helps-completed)))
