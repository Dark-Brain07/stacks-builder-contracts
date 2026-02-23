;; Community Garden Contract
;; Shared garden plot management
;; Halal - agriculture and community
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-ASSIGNED (err u405))

(define-data-var plot-count uint u0)
(define-data-var harvest-count uint u0)

(define-map plots uint { assignee: (optional principal), size-sqm: uint, crop: (string-utf8 50), season: (string-ascii 20), fee: uint, active: bool })
(define-map gardeners principal { plots-assigned: uint, total-harvests: uint, joined: uint })
(define-map harvests uint { plot-id: uint, gardener: principal, crop: (string-utf8 50), yield-kg: uint, block: uint })

(define-public (create-plot (size uint) (fee uint))
  (let ((id (+ (var-get plot-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set plots id { assignee: none, size-sqm: size, crop: u"", season: "available", fee: fee, active: true })
    (var-set plot-count id) (ok id)))

(define-public (assign-plot (plot-id uint))
  (let ((plot (unwrap! (map-get? plots plot-id) ERR-NOT-FOUND)))
    (asserts! (is-none (get assignee plot)) ERR-ALREADY-ASSIGNED)
    (if (> (get fee plot) u0) (try! (stx-transfer? (get fee plot) tx-sender CONTRACT-OWNER)) true)
    (map-set plots plot-id (merge plot { assignee: (some tx-sender), season: "assigned" }))
    (map-set gardeners tx-sender (merge (default-to { plots-assigned: u0, total-harvests: u0, joined: stacks-block-height } (map-get? gardeners tx-sender)) { plots-assigned: (+ (default-to u0 (get plots-assigned (map-get? gardeners tx-sender))) u1) }))
    (ok true)))

(define-public (plant-crop (plot-id uint) (crop (string-utf8 50)))
  (let ((plot (unwrap! (map-get? plots plot-id) ERR-NOT-FOUND)))
    (asserts! (match (get assignee plot) a (is-eq tx-sender a) false) ERR-NOT-AUTHORIZED)
    (map-set plots plot-id (merge plot { crop: crop, season: "planted" })) (ok true)))

(define-public (record-harvest (plot-id uint) (yield-kg uint))
  (let (
    (plot (unwrap! (map-get? plots plot-id) ERR-NOT-FOUND))
    (hid (+ (var-get harvest-count) u1))
    (gardener (default-to { plots-assigned: u0, total-harvests: u0, joined: u0 } (map-get? gardeners tx-sender)))
  )
    (asserts! (match (get assignee plot) a (is-eq tx-sender a) false) ERR-NOT-AUTHORIZED)
    (map-set harvests hid { plot-id: plot-id, gardener: tx-sender, crop: (get crop plot), yield-kg: yield-kg, block: stacks-block-height })
    (map-set plots plot-id (merge plot { season: "harvested" }))
    (map-set gardeners tx-sender (merge gardener { total-harvests: (+ (get total-harvests gardener) u1) }))
    (var-set harvest-count hid) (ok hid)))

(define-public (release-plot (plot-id uint))
  (let ((plot (unwrap! (map-get? plots plot-id) ERR-NOT-FOUND)))
    (asserts! (or (match (get assignee plot) a (is-eq tx-sender a) false) (is-eq tx-sender CONTRACT-OWNER)) ERR-NOT-AUTHORIZED)
    (map-set plots plot-id (merge plot { assignee: none, crop: u"", season: "available" })) (ok true)))

(define-read-only (get-plot (id uint)) (map-get? plots id))
(define-read-only (get-gardener (who principal)) (map-get? gardeners who))
(define-read-only (get-harvest (id uint)) (map-get? harvests id))
(define-read-only (get-plot-count) (ok (var-get plot-count)))
(define-read-only (get-harvest-count) (ok (var-get harvest-count)))
