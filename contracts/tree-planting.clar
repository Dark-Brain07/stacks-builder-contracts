;; Tree Planting Contract
;; Community tree planting initiative
;; Halal - environmental stewardship (khalifah)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var campaign-count uint u0)
(define-data-var total-trees uint u0)
(define-data-var total-funded uint u0)

(define-map campaigns uint {
  organizer: principal, name: (string-utf8 100), location: (string-utf8 100),
  goal-trees: uint, planted: uint, cost-per-tree: uint,
  funded: uint, planters: uint, status: (string-ascii 20), started: uint
})
(define-map planter-records { campaign-id: uint, planter: principal } { trees: uint, donated: uint })

(define-public (create-campaign (name (string-utf8 100)) (location (string-utf8 100)) (goal uint) (cost-per-tree uint))
  (let ((id (+ (var-get campaign-count) u1)))
    (map-set campaigns id { organizer: tx-sender, name: name, location: location, goal-trees: goal, planted: u0, cost-per-tree: cost-per-tree, funded: u0, planters: u0, status: "active", started: stacks-block-height })
    (var-set campaign-count id) (ok id)))

(define-public (fund-trees (campaign-id uint) (trees uint))
  (let (
    (camp (unwrap! (map-get? campaigns campaign-id) ERR-NOT-FOUND))
    (cost (* trees (get cost-per-tree camp)))
    (prev (default-to { trees: u0, donated: u0 } (map-get? planter-records { campaign-id: campaign-id, planter: tx-sender })))
    (is-new (is-eq (get trees prev) u0))
  )
    (try! (stx-transfer? cost tx-sender (get organizer camp)))
    (map-set planter-records { campaign-id: campaign-id, planter: tx-sender } { trees: (+ (get trees prev) trees), donated: (+ (get donated prev) cost) })
    (map-set campaigns campaign-id (merge camp { funded: (+ (get funded camp) cost), planters: (if is-new (+ (get planters camp) u1) (get planters camp)) }))
    (var-set total-funded (+ (var-get total-funded) cost))
    (ok trees)))

(define-public (record-planting (campaign-id uint) (trees-planted uint))
  (let ((camp (unwrap! (map-get? campaigns campaign-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer camp)) ERR-NOT-AUTHORIZED)
    (map-set campaigns campaign-id (merge camp { planted: (+ (get planted camp) trees-planted) }))
    (var-set total-trees (+ (var-get total-trees) trees-planted))
    (ok trees-planted)))

(define-public (complete-campaign (campaign-id uint))
  (let ((camp (unwrap! (map-get? campaigns campaign-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer camp)) ERR-NOT-AUTHORIZED)
    (map-set campaigns campaign-id (merge camp { status: "completed" })) (ok true)))

(define-read-only (get-campaign (id uint)) (map-get? campaigns id))
(define-read-only (get-planter (campaign-id uint) (planter principal)) (map-get? planter-records { campaign-id: campaign-id, planter: planter }))
(define-read-only (get-campaign-count) (ok (var-get campaign-count)))
(define-read-only (get-total-trees) (ok (var-get total-trees)))
(define-read-only (get-total-funded) (ok (var-get total-funded)))
