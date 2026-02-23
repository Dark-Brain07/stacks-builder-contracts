;; Review System Contract
;; On-chain reviews and ratings
;; Halal - honest feedback
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-REVIEWED (err u405))
(define-constant ERR-INVALID-RATING (err u406))
(define-constant ERR-SELF-REVIEW (err u407))

(define-data-var review-count uint u0)

(define-map reviews uint { reviewer: principal, subject: principal, rating: uint, comment: (string-utf8 200), created: uint })
(define-map subject-stats principal { total-rating: uint, review-count: uint, avg-rating: uint })
(define-map has-reviewed { reviewer: principal, subject: principal } bool)

(define-public (submit-review (subject principal) (rating uint) (comment (string-utf8 200)))
  (let (
    (id (+ (var-get review-count) u1))
    (stats (default-to { total-rating: u0, review-count: u0, avg-rating: u0 } (map-get? subject-stats subject)))
    (new-total (+ (get total-rating stats) rating))
    (new-count (+ (get review-count stats) u1))
  )
    (asserts! (not (is-eq tx-sender subject)) ERR-SELF-REVIEW)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
    (asserts! (is-none (map-get? has-reviewed { reviewer: tx-sender, subject: subject })) ERR-ALREADY-REVIEWED)
    (map-set reviews id { reviewer: tx-sender, subject: subject, rating: rating, comment: comment, created: stacks-block-height })
    (map-set has-reviewed { reviewer: tx-sender, subject: subject } true)
    (map-set subject-stats subject { total-rating: new-total, review-count: new-count, avg-rating: (/ new-total new-count) })
    (var-set review-count id) (ok id)))

(define-read-only (get-review (id uint)) (map-get? reviews id))
(define-read-only (get-stats (subject principal)) (map-get? subject-stats subject))
(define-read-only (get-review-count) (ok (var-get review-count)))
(define-read-only (has-user-reviewed (reviewer principal) (subject principal))
  (ok (default-to false (map-get? has-reviewed { reviewer: reviewer, subject: subject }))))
