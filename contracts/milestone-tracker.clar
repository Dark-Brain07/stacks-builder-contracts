;; Milestone Tracker Contract
;; Project milestone tracking and verification
;; Halal - transparent project management
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-NOT-VERIFIER (err u405))

(define-data-var project-count uint u0)
(define-data-var milestone-count uint u0)

(define-map projects uint { owner: principal, name: (string-utf8 100), milestones-total: uint, milestones-done: uint, status: (string-ascii 20), created: uint })
(define-map milestones uint { project-id: uint, title: (string-utf8 100), description: (string-utf8 200), verifier: principal, completed: bool, verified: bool, deadline: uint })
(define-map project-milestones { project-id: uint, index: uint } uint)

(define-public (create-project (name (string-utf8 100)))
  (let ((id (+ (var-get project-count) u1)))
    (map-set projects id { owner: tx-sender, name: name, milestones-total: u0, milestones-done: u0, status: "active", created: stacks-block-height })
    (var-set project-count id) (ok id)))

(define-public (add-milestone (project-id uint) (title (string-utf8 100)) (description (string-utf8 200)) (verifier principal) (deadline uint))
  (let (
    (project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND))
    (mid (+ (var-get milestone-count) u1))
    (idx (get milestones-total project))
  )
    (asserts! (is-eq tx-sender (get owner project)) ERR-NOT-AUTHORIZED)
    (map-set milestones mid { project-id: project-id, title: title, description: description, verifier: verifier, completed: false, verified: false, deadline: (+ stacks-block-height deadline) })
    (map-set project-milestones { project-id: project-id, index: idx } mid)
    (map-set projects project-id (merge project { milestones-total: (+ idx u1) }))
    (var-set milestone-count mid) (ok mid)))

(define-public (complete-milestone (milestone-id uint))
  (let ((ms (unwrap! (map-get? milestones milestone-id) ERR-NOT-FOUND))
    (project (unwrap! (map-get? projects (get project-id ms)) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get owner project)) ERR-NOT-AUTHORIZED)
    (map-set milestones milestone-id (merge ms { completed: true })) (ok true)))

(define-public (verify-milestone (milestone-id uint))
  (let (
    (ms (unwrap! (map-get? milestones milestone-id) ERR-NOT-FOUND))
    (project (unwrap! (map-get? projects (get project-id ms)) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get verifier ms)) ERR-NOT-VERIFIER)
    (asserts! (get completed ms) ERR-NOT-FOUND)
    (map-set milestones milestone-id (merge ms { verified: true }))
    (map-set projects (get project-id ms) (merge project { milestones-done: (+ (get milestones-done project) u1) }))
    (ok true)))

(define-public (close-project (project-id uint))
  (let ((project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner project)) ERR-NOT-AUTHORIZED)
    (map-set projects project-id (merge project { status: "closed" })) (ok true)))

(define-read-only (get-project (id uint)) (map-get? projects id))
(define-read-only (get-milestone (id uint)) (map-get? milestones id))
(define-read-only (get-project-milestone (project-id uint) (index uint)) (map-get? project-milestones { project-id: project-id, index: index }))
(define-read-only (get-project-count) (ok (var-get project-count)))
(define-read-only (get-milestone-count) (ok (var-get milestone-count)))
