;; Solar Panel Contract
;; Community solar panel sharing and credits
;; Halal - green energy stewardship
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var panel-count uint u0)
(define-data-var total-kwh uint u0)
(define-data-var total-invested uint u0)

(define-map panels uint { owner: principal, capacity-kw: uint, location: (string-utf8 100), investors: uint, total-invested: uint, total-generated: uint, active: bool })
(define-map panel-investors { panel-id: uint, investor: principal } { invested: uint, share-pct: uint, credits-earned: uint })
(define-map generation-log { panel-id: uint, index: uint } { kwh: uint, block: uint })
(define-map gen-count uint uint)

(define-public (install-panel (capacity uint) (location (string-utf8 100)))
  (let ((id (+ (var-get panel-count) u1)))
    (map-set panels id { owner: tx-sender, capacity-kw: capacity, location: location, investors: u0, total-invested: u0, total-generated: u0, active: true })
    (var-set panel-count id) (ok id)))

(define-public (invest-panel (panel-id uint) (amount uint) (share uint))
  (let ((panel (unwrap! (map-get? panels panel-id) ERR-NOT-FOUND)))
    (try! (stx-transfer? amount tx-sender (get owner panel)))
    (map-set panel-investors { panel-id: panel-id, investor: tx-sender } { invested: amount, share-pct: share, credits-earned: u0 })
    (map-set panels panel-id (merge panel { investors: (+ (get investors panel) u1), total-invested: (+ (get total-invested panel) amount) }))
    (var-set total-invested (+ (var-get total-invested) amount)) (ok amount)))

(define-public (record-generation (panel-id uint) (kwh uint))
  (let (
    (panel (unwrap! (map-get? panels panel-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? gen-count panel-id)))
  )
    (asserts! (is-eq tx-sender (get owner panel)) ERR-NOT-AUTHORIZED)
    (map-set generation-log { panel-id: panel-id, index: idx } { kwh: kwh, block: stacks-block-height })
    (map-set gen-count panel-id (+ idx u1))
    (map-set panels panel-id (merge panel { total-generated: (+ (get total-generated panel) kwh) }))
    (var-set total-kwh (+ (var-get total-kwh) kwh)) (ok kwh)))

(define-public (distribute-credits (panel-id uint) (investor principal) (credits uint))
  (let (
    (panel (unwrap! (map-get? panels panel-id) ERR-NOT-FOUND))
    (inv (unwrap! (map-get? panel-investors { panel-id: panel-id, investor: investor }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get owner panel)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? credits tx-sender investor))
    (map-set panel-investors { panel-id: panel-id, investor: investor } (merge inv { credits-earned: (+ (get credits-earned inv) credits) }))
    (ok credits)))

(define-read-only (get-panel (id uint)) (map-get? panels id))
(define-read-only (get-investor (panel-id uint) (investor principal)) (map-get? panel-investors { panel-id: panel-id, investor: investor }))
(define-read-only (get-generation (panel-id uint) (index uint)) (map-get? generation-log { panel-id: panel-id, index: index }))
(define-read-only (get-panel-count) (ok (var-get panel-count)))
(define-read-only (get-total-kwh) (ok (var-get total-kwh)))
