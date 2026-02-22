;; Property Share Contract
;; Fractional property ownership
;; Halal - asset-backed investment
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT (err u405))
(define-constant ERR-SOLD-OUT (err u406))

(define-data-var property-count uint u0)
(define-data-var total-invested uint u0)

(define-map properties uint {
  name: (string-utf8 100), location: (string-utf8 100), total-value: uint,
  share-price: uint, total-shares: uint, shares-sold: uint,
  rental-income: uint, active: bool
})
(define-map ownership { property-id: uint, owner: principal } { shares: uint, income-claimed: uint })

(define-public (list-property (name (string-utf8 100)) (location (string-utf8 100)) (value uint) (total-shares uint))
  (let ((id (+ (var-get property-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set properties id { name: name, location: location, total-value: value, share-price: (/ value total-shares), total-shares: total-shares, shares-sold: u0, rental-income: u0, active: true })
    (var-set property-count id) (ok id)))

(define-public (buy-shares (property-id uint) (num-shares uint))
  (let (
    (prop (unwrap! (map-get? properties property-id) ERR-NOT-FOUND))
    (cost (* num-shares (get share-price prop)))
    (current (default-to { shares: u0, income-claimed: u0 } (map-get? ownership { property-id: property-id, owner: tx-sender })))
  )
    (asserts! (get active prop) ERR-NOT-FOUND)
    (asserts! (<= (+ (get shares-sold prop) num-shares) (get total-shares prop)) ERR-SOLD-OUT)
    (try! (stx-transfer? cost tx-sender CONTRACT-OWNER))
    (map-set ownership { property-id: property-id, owner: tx-sender } { shares: (+ (get shares current) num-shares), income-claimed: (get income-claimed current) })
    (map-set properties property-id (merge prop { shares-sold: (+ (get shares-sold prop) num-shares) }))
    (var-set total-invested (+ (var-get total-invested) cost))
    (ok cost)))

(define-public (add-rental-income (property-id uint) (amount uint))
  (let ((prop (unwrap! (map-get? properties property-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set properties property-id (merge prop { rental-income: (+ (get rental-income prop) amount) }))
    (ok amount)))

(define-public (distribute-income (property-id uint) (owner principal) (amount uint))
  (let (
    (prop (unwrap! (map-get? properties property-id) ERR-NOT-FOUND))
    (own (unwrap! (map-get? ownership { property-id: property-id, owner: owner }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? amount tx-sender owner))
    (map-set ownership { property-id: property-id, owner: owner } (merge own { income-claimed: (+ (get income-claimed own) amount) }))
    (ok amount)))

(define-read-only (get-property (id uint)) (map-get? properties id))
(define-read-only (get-ownership (property-id uint) (owner principal)) (map-get? ownership { property-id: property-id, owner: owner }))
(define-read-only (get-property-count) (ok (var-get property-count)))
(define-read-only (get-total-invested) (ok (var-get total-invested)))
(define-read-only (get-share-income (property-id uint) (owner principal))
  (match (map-get? ownership { property-id: property-id, owner: owner })
    own (match (map-get? properties property-id)
      p (ok (/ (* (get rental-income p) (get shares own)) (get total-shares p)))
      (ok u0))
    (ok u0)))
