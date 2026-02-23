;; Halal Restaurant Contract
;; Halal restaurant directory and reviews
;; Halal - promoting halal food establishments
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-REVIEWED (err u405))

(define-data-var restaurant-count uint u0)
(define-data-var review-count uint u0)

(define-map restaurants uint {
  owner: principal, name: (string-utf8 100), cuisine: (string-utf8 50),
  location: (string-utf8 100), halal-certified: bool, certifier: (string-utf8 100),
  total-rating: uint, reviews: uint, avg-rating: uint, active: bool
})
(define-map restaurant-reviews uint { restaurant-id: uint, reviewer: principal, rating: uint, comment: (string-utf8 200), block: uint })
(define-map has-reviewed { restaurant-id: uint, reviewer: principal } bool)

(define-public (register-restaurant (name (string-utf8 100)) (cuisine (string-utf8 50)) (location (string-utf8 100)) (certifier (string-utf8 100)))
  (let ((id (+ (var-get restaurant-count) u1)))
    (map-set restaurants id { owner: tx-sender, name: name, cuisine: cuisine, location: location, halal-certified: false, certifier: certifier, total-rating: u0, reviews: u0, avg-rating: u0, active: true })
    (var-set restaurant-count id) (ok id)))

(define-public (certify-restaurant (restaurant-id uint))
  (let ((r (unwrap! (map-get? restaurants restaurant-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set restaurants restaurant-id (merge r { halal-certified: true })) (ok true)))

(define-public (review-restaurant (restaurant-id uint) (rating uint) (comment (string-utf8 200)))
  (let (
    (r (unwrap! (map-get? restaurants restaurant-id) ERR-NOT-FOUND))
    (rid (+ (var-get review-count) u1))
    (new-total (+ (get total-rating r) rating))
    (new-count (+ (get reviews r) u1))
  )
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? has-reviewed { restaurant-id: restaurant-id, reviewer: tx-sender })) ERR-ALREADY-REVIEWED)
    (map-set restaurant-reviews rid { restaurant-id: restaurant-id, reviewer: tx-sender, rating: rating, comment: comment, block: stacks-block-height })
    (map-set has-reviewed { restaurant-id: restaurant-id, reviewer: tx-sender } true)
    (map-set restaurants restaurant-id (merge r { total-rating: new-total, reviews: new-count, avg-rating: (/ new-total new-count) }))
    (var-set review-count rid) (ok rid)))

(define-public (deactivate-restaurant (restaurant-id uint))
  (let ((r (unwrap! (map-get? restaurants restaurant-id) ERR-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender (get owner r)) (is-eq tx-sender CONTRACT-OWNER)) ERR-NOT-AUTHORIZED)
    (map-set restaurants restaurant-id (merge r { active: false })) (ok true)))

(define-read-only (get-restaurant (id uint)) (map-get? restaurants id))
(define-read-only (get-review (id uint)) (map-get? restaurant-reviews id))
(define-read-only (get-restaurant-count) (ok (var-get restaurant-count)))
(define-read-only (get-review-count) (ok (var-get review-count)))
