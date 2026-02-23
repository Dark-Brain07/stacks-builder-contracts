;; Feedback System Contract
;; Anonymous on-chain feedback collection
;; Halal - constructive feedback (nasihah)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-SUBMITTED (err u405))

(define-data-var campaign-count uint u0)
(define-data-var feedback-count uint u0)

(define-map feedback-campaigns uint {
  creator: principal, title: (string-utf8 100), description: (string-utf8 200),
  responses: uint, avg-rating: uint, total-rating: uint, active: bool, created: uint
})
(define-map feedbacks uint { campaign-id: uint, rating: uint, comment: (string-utf8 200), block: uint })
(define-map has-responded { campaign-id: uint, responder: principal } bool)

(define-public (create-campaign (title (string-utf8 100)) (description (string-utf8 200)))
  (let ((id (+ (var-get campaign-count) u1)))
    (map-set feedback-campaigns id { creator: tx-sender, title: title, description: description, responses: u0, avg-rating: u0, total-rating: u0, active: true, created: stacks-block-height })
    (var-set campaign-count id) (ok id)))

(define-public (submit-feedback (campaign-id uint) (rating uint) (comment (string-utf8 200)))
  (let (
    (campaign (unwrap! (map-get? feedback-campaigns campaign-id) ERR-NOT-FOUND))
    (fid (+ (var-get feedback-count) u1))
    (new-total (+ (get total-rating campaign) rating))
    (new-responses (+ (get responses campaign) u1))
  )
    (asserts! (get active campaign) ERR-NOT-FOUND)
    (asserts! (is-none (map-get? has-responded { campaign-id: campaign-id, responder: tx-sender })) ERR-ALREADY-SUBMITTED)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-NOT-AUTHORIZED)
    (map-set feedbacks fid { campaign-id: campaign-id, rating: rating, comment: comment, block: stacks-block-height })
    (map-set has-responded { campaign-id: campaign-id, responder: tx-sender } true)
    (map-set feedback-campaigns campaign-id (merge campaign { responses: new-responses, total-rating: new-total, avg-rating: (/ new-total new-responses) }))
    (var-set feedback-count fid) (ok fid)))

(define-public (close-campaign (campaign-id uint))
  (let ((campaign (unwrap! (map-get? feedback-campaigns campaign-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get creator campaign)) ERR-NOT-AUTHORIZED)
    (map-set feedback-campaigns campaign-id (merge campaign { active: false })) (ok true)))

(define-read-only (get-campaign (id uint)) (map-get? feedback-campaigns id))
(define-read-only (get-feedback (id uint)) (map-get? feedbacks id))
(define-read-only (get-campaign-count) (ok (var-get campaign-count)))
(define-read-only (get-feedback-count) (ok (var-get feedback-count)))
(define-read-only (has-submitted (campaign-id uint) (user principal)) (ok (default-to false (map-get? has-responded { campaign-id: campaign-id, responder: user }))))
