;; Escrow Milestone Contract
;; Multi-milestone escrow with staged releases
;; Halal - fair project payments
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-RELEASED (err u405))

(define-data-var project-count uint u0)
(define-data-var total-escrowed uint u0)

(define-map projects uint {
  client: principal, provider: principal, total-value: uint,
  released: uint, milestone-count: uint, status: (string-ascii 20), created: uint
})
(define-map milestones { project-id: uint, index: uint } {
  description: (string-utf8 200), amount: uint, status: (string-ascii 20), approved-at: uint
})

(define-public (create-project (provider principal) (total-value uint))
  (let ((id (+ (var-get project-count) u1)))
    (try! (stx-transfer? total-value tx-sender CONTRACT-OWNER))
    (map-set projects id { client: tx-sender, provider: provider, total-value: total-value, released: u0, milestone-count: u0, status: "active", created: stacks-block-height })
    (var-set project-count id)
    (var-set total-escrowed (+ (var-get total-escrowed) total-value))
    (ok id)))

(define-public (add-milestone (project-id uint) (description (string-utf8 200)) (amount uint))
  (let (
    (project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND))
    (idx (get milestone-count project))
  )
    (asserts! (is-eq tx-sender (get client project)) ERR-NOT-AUTHORIZED)
    (map-set milestones { project-id: project-id, index: idx } { description: description, amount: amount, status: "pending", approved-at: u0 })
    (map-set projects project-id (merge project { milestone-count: (+ idx u1) }))
    (ok idx)))

(define-public (submit-milestone (project-id uint) (milestone-index uint))
  (let (
    (project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND))
    (ms (unwrap! (map-get? milestones { project-id: project-id, index: milestone-index }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get provider project)) ERR-NOT-AUTHORIZED)
    (map-set milestones { project-id: project-id, index: milestone-index } (merge ms { status: "submitted" }))
    (ok true)))

(define-public (approve-milestone (project-id uint) (milestone-index uint))
  (let (
    (project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND))
    (ms (unwrap! (map-get? milestones { project-id: project-id, index: milestone-index }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get client project)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status ms) "submitted") ERR-ALREADY-RELEASED)
    (try! (stx-transfer? (get amount ms) CONTRACT-OWNER (get provider project)))
    (map-set milestones { project-id: project-id, index: milestone-index } (merge ms { status: "approved", approved-at: stacks-block-height }))
    (map-set projects project-id (merge project { released: (+ (get released project) (get amount ms)) }))
    (ok (get amount ms))))

(define-public (cancel-project (project-id uint))
  (let (
    (project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND))
    (remaining (- (get total-value project) (get released project)))
  )
    (asserts! (is-eq tx-sender (get client project)) ERR-NOT-AUTHORIZED)
    (if (> remaining u0) (begin (try! (stx-transfer? remaining CONTRACT-OWNER (get client project))) true) true)
    (map-set projects project-id (merge project { status: "cancelled" })) (ok remaining)))

(define-read-only (get-project (id uint)) (map-get? projects id))
(define-read-only (get-milestone (project-id uint) (index uint)) (map-get? milestones { project-id: project-id, index: index }))
(define-read-only (get-project-count) (ok (var-get project-count)))
(define-read-only (get-total-escrowed) (ok (var-get total-escrowed)))
