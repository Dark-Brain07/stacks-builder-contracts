;; Farm Share Contract
;; Community-supported agriculture (CSA)
;; Halal - permissible farming and food
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-FULL (err u405))

(define-data-var farm-count uint u0)
(define-data-var total-members uint u0)

(define-map farms uint { farmer: principal, name: (string-utf8 100), location: (string-utf8 100), max-shares: uint, shares-sold: uint, share-price: uint, season: (string-ascii 20), active: bool })
(define-map farm-members { farm-id: uint, member: principal } { shares: uint, paid: uint, pickups: uint, joined: uint })
(define-map harvest-logs { farm-id: uint, week: uint } { items: (string-utf8 200), quantity-kg: uint })

(define-public (register-farm (name (string-utf8 100)) (location (string-utf8 100)) (max-shares uint) (share-price uint) (season (string-ascii 20)))
  (let ((id (+ (var-get farm-count) u1)))
    (map-set farms id { farmer: tx-sender, name: name, location: location, max-shares: max-shares, shares-sold: u0, share-price: share-price, season: season, active: true })
    (var-set farm-count id) (ok id)))

(define-public (buy-share (farm-id uint) (shares uint))
  (let (
    (farm (unwrap! (map-get? farms farm-id) ERR-NOT-FOUND))
    (cost (* shares (get share-price farm)))
  )
    (asserts! (get active farm) ERR-NOT-FOUND)
    (asserts! (<= (+ (get shares-sold farm) shares) (get max-shares farm)) ERR-FULL)
    (try! (stx-transfer? cost tx-sender (get farmer farm)))
    (map-set farm-members { farm-id: farm-id, member: tx-sender } { shares: shares, paid: cost, pickups: u0, joined: stacks-block-height })
    (map-set farms farm-id (merge farm { shares-sold: (+ (get shares-sold farm) shares) }))
    (var-set total-members (+ (var-get total-members) u1)) (ok cost)))

(define-public (log-harvest (farm-id uint) (week uint) (items (string-utf8 200)) (quantity uint))
  (let ((farm (unwrap! (map-get? farms farm-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get farmer farm)) ERR-NOT-AUTHORIZED)
    (map-set harvest-logs { farm-id: farm-id, week: week } { items: items, quantity-kg: quantity }) (ok week)))

(define-public (record-pickup (farm-id uint) (member principal))
  (let (
    (farm (unwrap! (map-get? farms farm-id) ERR-NOT-FOUND))
    (m (unwrap! (map-get? farm-members { farm-id: farm-id, member: member }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get farmer farm)) ERR-NOT-AUTHORIZED)
    (map-set farm-members { farm-id: farm-id, member: member } (merge m { pickups: (+ (get pickups m) u1) })) (ok true)))

(define-public (end-season (farm-id uint))
  (let ((farm (unwrap! (map-get? farms farm-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get farmer farm)) ERR-NOT-AUTHORIZED)
    (map-set farms farm-id (merge farm { active: false })) (ok true)))

(define-read-only (get-farm (id uint)) (map-get? farms id))
(define-read-only (get-member (farm-id uint) (who principal)) (map-get? farm-members { farm-id: farm-id, member: who }))
(define-read-only (get-harvest (farm-id uint) (week uint)) (map-get? harvest-logs { farm-id: farm-id, week: week }))
(define-read-only (get-farm-count) (ok (var-get farm-count)))
(define-read-only (get-total-members) (ok (var-get total-members)))
