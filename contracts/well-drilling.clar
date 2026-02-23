;; Well Drilling Contract
;; Water well project funding and tracking
;; Halal - sadaqah jariyah (ongoing charity)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var well-count uint u0)
(define-data-var total-raised uint u0)
(define-data-var wells-completed uint u0)

(define-map wells uint {
  sponsor: principal, location: (string-utf8 100), community: (string-utf8 100),
  cost: uint, raised: uint, donors: uint, beneficiaries: uint,
  drilling-status: (string-ascii 20), depth-meters: uint, created: uint
})
(define-map well-donors { well-id: uint, donor: principal } uint)
(define-map well-updates { well-id: uint, index: uint } { update: (string-utf8 200), photos-hash: (buff 32), block: uint })
(define-map update-count uint uint)

(define-public (create-well-project (location (string-utf8 100)) (community (string-utf8 100)) (cost uint) (beneficiaries uint))
  (let ((id (+ (var-get well-count) u1)))
    (map-set wells id { sponsor: tx-sender, location: location, community: community, cost: cost, raised: u0, donors: u0, beneficiaries: beneficiaries, drilling-status: "planned", depth-meters: u0, created: stacks-block-height })
    (var-set well-count id) (ok id)))

(define-public (donate-to-well (well-id uint) (amount uint))
  (let (
    (well (unwrap! (map-get? wells well-id) ERR-NOT-FOUND))
    (prev (default-to u0 (map-get? well-donors { well-id: well-id, donor: tx-sender })))
    (is-new (is-eq prev u0))
  )
    (try! (stx-transfer? amount tx-sender (get sponsor well)))
    (map-set well-donors { well-id: well-id, donor: tx-sender } (+ prev amount))
    (map-set wells well-id (merge well { raised: (+ (get raised well) amount), donors: (if is-new (+ (get donors well) u1) (get donors well)) }))
    (var-set total-raised (+ (var-get total-raised) amount))
    (print { event: "well-donation", well: well-id, donor: tx-sender, amount: amount })
    (ok amount)))

(define-public (update-progress (well-id uint) (status (string-ascii 20)) (depth uint) (update-text (string-utf8 200)) (photos (buff 32)))
  (let (
    (well (unwrap! (map-get? wells well-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? update-count well-id)))
  )
    (asserts! (is-eq tx-sender (get sponsor well)) ERR-NOT-AUTHORIZED)
    (map-set well-updates { well-id: well-id, index: idx } { update: update-text, photos-hash: photos, block: stacks-block-height })
    (map-set update-count well-id (+ idx u1))
    (map-set wells well-id (merge well { drilling-status: status, depth-meters: depth }))
    (if (is-eq status "completed") (var-set wells-completed (+ (var-get wells-completed) u1)) true)
    (ok idx)))

(define-read-only (get-well (id uint)) (map-get? wells id))
(define-read-only (get-donation (well-id uint) (donor principal)) (ok (default-to u0 (map-get? well-donors { well-id: well-id, donor: donor }))))
(define-read-only (get-update (well-id uint) (index uint)) (map-get? well-updates { well-id: well-id, index: index }))
(define-read-only (get-well-count) (ok (var-get well-count)))
(define-read-only (get-total-raised) (ok (var-get total-raised)))
(define-read-only (get-wells-completed) (ok (var-get wells-completed)))
