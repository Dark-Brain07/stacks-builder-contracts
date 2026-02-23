;; Coupon System Contract
;; Digital coupon issuance and redemption
;; Halal - fair discounts
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-EXPIRED (err u405))
(define-constant ERR-ALREADY-USED (err u406))
(define-constant ERR-LIMIT-REACHED (err u407))

(define-data-var coupon-count uint u0)
(define-data-var total-redeemed uint u0)

(define-map coupons uint {
  creator: principal, code: (string-ascii 20), discount-pct: uint,
  max-uses: uint, used: uint, expires: uint, active: bool
})
(define-map coupon-uses { coupon-id: uint, user: principal } bool)
(define-map merchant-coupons principal uint)

(define-public (create-coupon (code (string-ascii 20)) (discount uint) (max-uses uint) (validity uint))
  (let ((id (+ (var-get coupon-count) u1)))
    (map-set coupons id {
      creator: tx-sender, code: code, discount-pct: discount,
      max-uses: max-uses, used: u0, expires: (+ stacks-block-height validity), active: true
    })
    (map-set merchant-coupons tx-sender (+ (default-to u0 (map-get? merchant-coupons tx-sender)) u1))
    (var-set coupon-count id) (ok id)))

(define-public (redeem-coupon (coupon-id uint))
  (let ((coupon (unwrap! (map-get? coupons coupon-id) ERR-NOT-FOUND)))
    (asserts! (get active coupon) ERR-NOT-FOUND)
    (asserts! (< stacks-block-height (get expires coupon)) ERR-EXPIRED)
    (asserts! (< (get used coupon) (get max-uses coupon)) ERR-LIMIT-REACHED)
    (asserts! (is-none (map-get? coupon-uses { coupon-id: coupon-id, user: tx-sender })) ERR-ALREADY-USED)
    (map-set coupon-uses { coupon-id: coupon-id, user: tx-sender } true)
    (map-set coupons coupon-id (merge coupon { used: (+ (get used coupon) u1) }))
    (var-set total-redeemed (+ (var-get total-redeemed) u1))
    (print { event: "coupon-redeemed", id: coupon-id, user: tx-sender, discount: (get discount-pct coupon) })
    (ok (get discount-pct coupon))))

(define-public (deactivate-coupon (coupon-id uint))
  (let ((coupon (unwrap! (map-get? coupons coupon-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get creator coupon)) ERR-NOT-AUTHORIZED)
    (map-set coupons coupon-id (merge coupon { active: false })) (ok true)))

(define-read-only (get-coupon (id uint)) (map-get? coupons id))
(define-read-only (get-coupon-count) (ok (var-get coupon-count)))
(define-read-only (get-total-redeemed) (ok (var-get total-redeemed)))
(define-read-only (has-used (coupon-id uint) (user principal)) (ok (default-to false (map-get? coupon-uses { coupon-id: coupon-id, user: user }))))
(define-read-only (is-valid (id uint))
  (match (map-get? coupons id) c (ok (and (get active c) (< stacks-block-height (get expires c)) (< (get used c) (get max-uses c)))) (ok false)))
