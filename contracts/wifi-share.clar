;; WiFi Share Contract
;; Community WiFi sharing network
;; Halal - connectivity sharing
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var hotspot-count uint u0)
(define-data-var connection-count uint u0)
(define-data-var total-revenue uint u0)

(define-map hotspots uint { provider: principal, name: (string-utf8 100), location: (string-utf8 100), speed-mbps: uint, price-per-hour: uint, users: uint, active: bool })
(define-map connections uint { hotspot-id: uint, user: principal, hours: uint, paid: uint, started: uint, status: (string-ascii 20) })
(define-map provider-stats principal { hotspots: uint, total-users: uint, earned: uint })

(define-public (register-hotspot (name (string-utf8 100)) (location (string-utf8 100)) (speed uint) (price uint))
  (let (
    (id (+ (var-get hotspot-count) u1))
    (stats (default-to { hotspots: u0, total-users: u0, earned: u0 } (map-get? provider-stats tx-sender)))
  )
    (map-set hotspots id { provider: tx-sender, name: name, location: location, speed-mbps: speed, price-per-hour: price, users: u0, active: true })
    (map-set provider-stats tx-sender (merge stats { hotspots: (+ (get hotspots stats) u1) }))
    (var-set hotspot-count id) (ok id)))

(define-public (connect (hotspot-id uint) (hours uint))
  (let (
    (hs (unwrap! (map-get? hotspots hotspot-id) ERR-NOT-FOUND))
    (cost (* hours (get price-per-hour hs)))
    (cid (+ (var-get connection-count) u1))
    (stats (default-to { hotspots: u0, total-users: u0, earned: u0 } (map-get? provider-stats (get provider hs))))
  )
    (asserts! (get active hs) ERR-NOT-FOUND)
    (try! (stx-transfer? cost tx-sender (get provider hs)))
    (map-set connections cid { hotspot-id: hotspot-id, user: tx-sender, hours: hours, paid: cost, started: stacks-block-height, status: "active" })
    (map-set hotspots hotspot-id (merge hs { users: (+ (get users hs) u1) }))
    (map-set provider-stats (get provider hs) (merge stats { total-users: (+ (get total-users stats) u1), earned: (+ (get earned stats) cost) }))
    (var-set connection-count cid)
    (var-set total-revenue (+ (var-get total-revenue) cost)) (ok cid)))

(define-public (disconnect (connection-id uint))
  (let ((c (unwrap! (map-get? connections connection-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get user c)) ERR-NOT-AUTHORIZED)
    (map-set connections connection-id (merge c { status: "ended" })) (ok true)))

(define-public (toggle-hotspot (hotspot-id uint) (active bool))
  (let ((hs (unwrap! (map-get? hotspots hotspot-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get provider hs)) ERR-NOT-AUTHORIZED)
    (map-set hotspots hotspot-id (merge hs { active: active })) (ok true)))

(define-read-only (get-hotspot (id uint)) (map-get? hotspots id))
(define-read-only (get-connection (id uint)) (map-get? connections id))
(define-read-only (get-provider (who principal)) (map-get? provider-stats who))
(define-read-only (get-hotspot-count) (ok (var-get hotspot-count)))
(define-read-only (get-total-revenue) (ok (var-get total-revenue)))
