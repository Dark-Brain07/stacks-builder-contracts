;; Vendor Rating Contract
;; Vendor/merchant reputation system
;; Halal - honest commerce
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-RATED (err u405))

(define-data-var vendor-count uint u0)
(define-data-var review-count uint u0)

(define-map vendors principal { name: (string-utf8 100), category: (string-ascii 20), total-rating: uint, review-count: uint, avg-rating: uint, verified: bool, registered: uint })
(define-map reviews uint { vendor: principal, reviewer: principal, rating: uint, comment: (string-utf8 200), block: uint })
(define-map has-reviewed { vendor: principal, reviewer: principal } bool)

(define-public (register-vendor (name (string-utf8 100)) (category (string-ascii 20)))
  (begin
    (map-set vendors tx-sender { name: name, category: category, total-rating: u0, review-count: u0, avg-rating: u0, verified: false, registered: stacks-block-height })
    (var-set vendor-count (+ (var-get vendor-count) u1)) (ok true)))

(define-public (verify-vendor (vendor principal))
  (let ((v (unwrap! (map-get? vendors vendor) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set vendors vendor (merge v { verified: true })) (ok true)))

(define-public (rate-vendor (vendor principal) (rating uint) (comment (string-utf8 200)))
  (let (
    (v (unwrap! (map-get? vendors vendor) ERR-NOT-FOUND))
    (rid (+ (var-get review-count) u1))
    (new-total (+ (get total-rating v) rating))
    (new-count (+ (get review-count v) u1))
  )
    (asserts! (not (is-eq tx-sender vendor)) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? has-reviewed { vendor: vendor, reviewer: tx-sender })) ERR-ALREADY-RATED)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-NOT-AUTHORIZED)
    (map-set reviews rid { vendor: vendor, reviewer: tx-sender, rating: rating, comment: comment, block: stacks-block-height })
    (map-set has-reviewed { vendor: vendor, reviewer: tx-sender } true)
    (map-set vendors vendor (merge v { total-rating: new-total, review-count: new-count, avg-rating: (/ new-total new-count) }))
    (var-set review-count rid) (ok rid)))

(define-public (respond-review (review-id uint) (response (string-utf8 200)))
  (let ((r (unwrap! (map-get? reviews review-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get vendor r)) ERR-NOT-AUTHORIZED)
    (ok true)))

(define-read-only (get-vendor (who principal)) (map-get? vendors who))
(define-read-only (get-review (id uint)) (map-get? reviews id))
(define-read-only (get-vendor-count) (ok (var-get vendor-count)))
(define-read-only (get-review-count) (ok (var-get review-count)))
(define-read-only (has-reviewed-vendor (vendor principal) (reviewer principal)) (ok (default-to false (map-get? has-reviewed { vendor: vendor, reviewer: reviewer }))))
