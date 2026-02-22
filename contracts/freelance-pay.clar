;; Freelance Payment Contract
;; Milestone-based payments for freelance work
;; Halal - fair work compensation
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-DONE (err u405))
(define-constant ERR-NOT-CLIENT (err u406))
(define-constant ERR-NOT-FREELANCER (err u407))

(define-data-var project-count uint u0)
(define-data-var total-paid uint u0)

(define-map projects uint {
  client: principal, freelancer: principal, title: (string-utf8 100),
  total-budget: uint, paid: uint, milestones: uint, completed: uint, status: (string-ascii 20)
})
(define-map milestones { project-id: uint, index: uint } { description: (string-utf8 200), amount: uint, completed: bool, approved: bool })

(define-public (create-project (freelancer principal) (title (string-utf8 100)) (budget uint) (milestone-count uint))
  (let ((id (+ (var-get project-count) u1)))
    (map-set projects id {
      client: tx-sender, freelancer: freelancer, title: title,
      total-budget: budget, paid: u0, milestones: milestone-count, completed: u0, status: "active"
    })
    (var-set project-count id) (ok id)))

(define-public (add-milestone (project-id uint) (index uint) (description (string-utf8 200)) (amount uint))
  (let ((project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get client project)) ERR-NOT-CLIENT)
    (map-set milestones { project-id: project-id, index: index } { description: description, amount: amount, completed: false, approved: false })
    (ok true)))

(define-public (complete-milestone (project-id uint) (index uint))
  (let (
    (project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND))
    (ms (unwrap! (map-get? milestones { project-id: project-id, index: index }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get freelancer project)) ERR-NOT-FREELANCER)
    (asserts! (not (get completed ms)) ERR-ALREADY-DONE)
    (map-set milestones { project-id: project-id, index: index } (merge ms { completed: true }))
    (ok true)))

(define-public (approve-and-pay (project-id uint) (index uint))
  (let (
    (project (unwrap! (map-get? projects project-id) ERR-NOT-FOUND))
    (ms (unwrap! (map-get? milestones { project-id: project-id, index: index }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get client project)) ERR-NOT-CLIENT)
    (asserts! (get completed ms) ERR-NOT-FOUND)
    (asserts! (not (get approved ms)) ERR-ALREADY-DONE)
    (try! (stx-transfer? (get amount ms) tx-sender (get freelancer project)))
    (map-set milestones { project-id: project-id, index: index } (merge ms { approved: true }))
    (map-set projects project-id (merge project { paid: (+ (get paid project) (get amount ms)), completed: (+ (get completed project) u1) }))
    (var-set total-paid (+ (var-get total-paid) (get amount ms)))
    (ok true)))

(define-read-only (get-project (id uint)) (map-get? projects id))
(define-read-only (get-milestone (project-id uint) (index uint)) (map-get? milestones { project-id: project-id, index: index }))
(define-read-only (get-project-count) (ok (var-get project-count)))
(define-read-only (get-total-paid) (ok (var-get total-paid)))
