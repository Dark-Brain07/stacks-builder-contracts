;; Project Funding Contract
;; Milestone-based project funding
;; Halal - transparent project finance
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-GOAL-MET (err u405))

(define-data-var project-count uint u0)
(define-data-var total-funded uint u0)

(define-map projects uint {
  creator: principal, title: (string-utf8 100), description: (string-utf8 200),
  funding-goal: uint, raised: uint, backers: uint,
  status: (string-ascii 20), deadline: uint, created: uint
})
(define-map project-backers { project-id: uint, backer: principal } { amount: uint, backed-at: uint })
(define-map project-updates { project-id: uint, index: uint } { update: (string-utf8 200), block: uint })
(define-map update-count uint uint)

(define-public (create-project (title (string-utf8 100)) (description (string-utf8 200)) (goal uint) (duration uint))
  (let ((id (+ (var-get project-count) u1)))
    (map-set projects id {
      creator: tx-sender, title: title, description: description,
      funding-goal: goal, raised: u0, backers: u0,
      status: "active", deadline: (+ stacks-block-height duration), created: stacks-block-height
    })
    (var-set project-count id) (ok id)))

(define-public (back-project (project-id uint) (amount uint))
  (let (
    (project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND))
    (prev (default-to { amount: u0, backed-at: u0 } (map-get? project-backers { project-id: project-id, backer: tx-sender })))
    (is-new (is-eq (get amount prev) u0))
  )
    (asserts! (is-eq (get status project) "active") ERR-NOT-FOUND)
    (asserts! (< stacks-block-height (get deadline project)) ERR-NOT-FOUND)
    (try! (stx-transfer? amount tx-sender (get creator project)))
    (map-set project-backers { project-id: project-id, backer: tx-sender } { amount: (+ (get amount prev) amount), backed-at: stacks-block-height })
    (map-set projects project-id (merge project { raised: (+ (get raised project) amount), backers: (if is-new (+ (get backers project) u1) (get backers project)) }))
    (var-set total-funded (+ (var-get total-funded) amount))
    (ok amount)))

(define-public (post-update (project-id uint) (update-text (string-utf8 200)))
  (let (
    (project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? update-count project-id)))
  )
    (asserts! (is-eq tx-sender (get creator project)) ERR-NOT-AUTHORIZED)
    (map-set project-updates { project-id: project-id, index: idx } { update: update-text, block: stacks-block-height })
    (map-set update-count project-id (+ idx u1)) (ok idx)))

(define-public (complete-project (project-id uint))
  (let ((project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get creator project)) ERR-NOT-AUTHORIZED)
    (map-set projects project-id (merge project { status: "completed" })) (ok true)))

(define-read-only (get-project (id uint)) (map-get? projects id))
(define-read-only (get-backer (project-id uint) (backer principal)) (map-get? project-backers { project-id: project-id, backer: backer }))
(define-read-only (get-update (project-id uint) (index uint)) (map-get? project-updates { project-id: project-id, index: index }))
(define-read-only (get-project-count) (ok (var-get project-count)))
(define-read-only (get-total-funded) (ok (var-get total-funded)))
