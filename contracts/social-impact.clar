;; Social Impact Contract
;; Track and verify social impact projects
;; Halal - community benefit
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var project-count uint u0)
(define-data-var milestone-total uint u0)
(define-data-var total-impact-funded uint u0)

(define-map projects uint {
  creator: principal, title: (string-utf8 100), category: (string-ascii 30),
  target-impact: uint, current-impact: uint, budget: uint, spent: uint, status: (string-ascii 20), created: uint
})
(define-map impact-milestones { project-id: uint, index: uint } { description: (string-utf8 200), target: uint, achieved: uint, verified: bool })
(define-map project-milestone-count uint uint)
(define-map impact-funders { project-id: uint, funder: principal } uint)

(define-public (create-project (title (string-utf8 100)) (category (string-ascii 30)) (target uint) (budget uint))
  (let ((id (+ (var-get project-count) u1)))
    (map-set projects id {
      creator: tx-sender, title: title, category: category,
      target-impact: target, current-impact: u0, budget: budget, spent: u0, status: "active", created: stacks-block-height
    })
    (var-set project-count id) (ok id)))

(define-public (fund-project (project-id uint) (amount uint))
  (let (
    (project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND))
    (prev (default-to u0 (map-get? impact-funders { project-id: project-id, funder: tx-sender })))
  )
    (try! (stx-transfer? amount tx-sender (get creator project)))
    (map-set impact-funders { project-id: project-id, funder: tx-sender } (+ prev amount))
    (var-set total-impact-funded (+ (var-get total-impact-funded) amount))
    (ok amount)))

(define-public (add-milestone (project-id uint) (description (string-utf8 200)) (target uint))
  (let (
    (project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? project-milestone-count project-id)))
  )
    (asserts! (is-eq tx-sender (get creator project)) ERR-NOT-AUTHORIZED)
    (map-set impact-milestones { project-id: project-id, index: idx } { description: description, target: target, achieved: u0, verified: false })
    (map-set project-milestone-count project-id (+ idx u1))
    (var-set milestone-total (+ (var-get milestone-total) u1)) (ok idx)))

(define-public (update-impact (project-id uint) (milestone-index uint) (achieved uint))
  (let (
    (project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND))
    (ms (unwrap! (map-get? impact-milestones { project-id: project-id, index: milestone-index }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get creator project)) ERR-NOT-AUTHORIZED)
    (map-set impact-milestones { project-id: project-id, index: milestone-index } (merge ms { achieved: achieved }))
    (map-set projects project-id (merge project { current-impact: (+ (get current-impact project) achieved) }))
    (ok achieved)))

(define-public (verify-milestone (project-id uint) (milestone-index uint))
  (let ((ms (unwrap! (map-get? impact-milestones { project-id: project-id, index: milestone-index }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set impact-milestones { project-id: project-id, index: milestone-index } (merge ms { verified: true }))
    (ok true)))

(define-read-only (get-project (id uint)) (map-get? projects id))
(define-read-only (get-milestone (project-id uint) (index uint)) (map-get? impact-milestones { project-id: project-id, index: index }))
(define-read-only (get-project-count) (ok (var-get project-count)))
(define-read-only (get-total-funded) (ok (var-get total-impact-funded)))
(define-read-only (get-funder-amount (project-id uint) (funder principal)) (ok (default-to u0 (map-get? impact-funders { project-id: project-id, funder: funder }))))
