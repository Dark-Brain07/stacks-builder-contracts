;; Recycling Rewards Contract
;; Incentivize recycling with token rewards
;; Halal - environmental stewardship
;; Clarity 4 compatible

(define-fungible-token recycle-point)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-NOT-CENTER (err u405))

(define-data-var total-recycled uint u0)
(define-data-var center-count uint u0)

(define-map recycling-centers principal { name: (string-utf8 100), location: (string-utf8 100), verified: bool, items-processed: uint })
(define-map recycler-stats principal { items: uint, points-earned: uint, redemptions: uint })
(define-map reward-tiers (string-ascii 20) uint)

(map-set reward-tiers "plastic" u10)
(map-set reward-tiers "paper" u5)
(map-set reward-tiers "glass" u15)
(map-set reward-tiers "metal" u20)
(map-set reward-tiers "electronics" u50)

(define-public (register-center (name (string-utf8 100)) (location (string-utf8 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set recycling-centers tx-sender { name: name, location: location, verified: true, items-processed: u0 })
    (var-set center-count (+ (var-get center-count) u1)) (ok true)))

(define-public (add-center (center principal) (name (string-utf8 100)) (location (string-utf8 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set recycling-centers center { name: name, location: location, verified: true, items-processed: u0 })
    (var-set center-count (+ (var-get center-count) u1)) (ok true)))

(define-public (recycle-item (recycler principal) (material (string-ascii 20)) (quantity uint))
  (let (
    (center (unwrap! (map-get? recycling-centers tx-sender) ERR-NOT-CENTER))
    (points-per (default-to u5 (map-get? reward-tiers material)))
    (total-points (* points-per quantity))
    (stats (default-to { items: u0, points-earned: u0, redemptions: u0 } (map-get? recycler-stats recycler)))
  )
    (asserts! (get verified center) ERR-NOT-CENTER)
    (try! (ft-mint? recycle-point total-points recycler))
    (map-set recycler-stats recycler (merge stats { items: (+ (get items stats) quantity), points-earned: (+ (get points-earned stats) total-points) }))
    (map-set recycling-centers tx-sender (merge center { items-processed: (+ (get items-processed center) quantity) }))
    (var-set total-recycled (+ (var-get total-recycled) quantity))
    (ok total-points)))

(define-public (redeem-points (amount uint))
  (let ((stats (default-to { items: u0, points-earned: u0, redemptions: u0 } (map-get? recycler-stats tx-sender))))
    (try! (ft-burn? recycle-point amount tx-sender))
    (map-set recycler-stats tx-sender (merge stats { redemptions: (+ (get redemptions stats) u1) }))
    (ok amount)))

(define-read-only (get-balance (who principal)) (ok (ft-get-balance recycle-point who)))
(define-read-only (get-center (who principal)) (map-get? recycling-centers who))
(define-read-only (get-recycler (who principal)) (map-get? recycler-stats who))
(define-read-only (get-reward-rate (material (string-ascii 20))) (ok (default-to u0 (map-get? reward-tiers material))))
(define-read-only (get-total-recycled) (ok (var-get total-recycled)))
