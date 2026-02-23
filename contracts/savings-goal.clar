;; Savings Goal Contract
;; Set and track personal savings goals
;; Halal - responsible financial planning
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-GOAL-REACHED (err u405))

(define-data-var goal-count uint u0)

(define-map savings-goals uint { owner: principal, name: (string-utf8 100), target: uint, saved: uint, deadline: uint, reached: bool })
(define-map deposits { goal-id: uint, index: uint } { amount: uint, block: uint })
(define-map deposit-count uint uint)

(define-public (create-goal (name (string-utf8 100)) (target uint) (deadline uint))
  (let ((id (+ (var-get goal-count) u1)))
    (map-set savings-goals id { owner: tx-sender, name: name, target: target, saved: u0, deadline: (+ stacks-block-height deadline), reached: false })
    (var-set goal-count id) (ok id)))

(define-public (add-savings (goal-id uint) (amount uint))
  (let (
    (goal (unwrap! (map-get? savings-goals goal-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? deposit-count goal-id)))
    (new-saved (+ (get saved goal) amount))
  )
    (asserts! (is-eq tx-sender (get owner goal)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set deposits { goal-id: goal-id, index: idx } { amount: amount, block: stacks-block-height })
    (map-set deposit-count goal-id (+ idx u1))
    (map-set savings-goals goal-id (merge goal { saved: new-saved, reached: (>= new-saved (get target goal)) }))
    (ok new-saved)))

(define-public (withdraw-savings (goal-id uint))
  (let ((goal (unwrap! (map-get? savings-goals goal-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? (get saved goal) tx-sender (get owner goal)))
    (map-set savings-goals goal-id (merge goal { saved: u0 })) (ok (get saved goal))))

(define-read-only (get-goal (id uint)) (map-get? savings-goals id))
(define-read-only (get-goal-count) (ok (var-get goal-count)))
(define-read-only (get-deposit (goal-id uint) (index uint)) (map-get? deposits { goal-id: goal-id, index: index }))
(define-read-only (get-progress (id uint))
  (match (map-get? savings-goals id) g (ok (/ (* (get saved g) u100) (get target g))) (ok u0)))
