;; Hajj Savings Contract
;; Savings plan for Hajj pilgrimage
;; Halal - facilitating obligatory pilgrimage
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-GOAL-NOT-MET (err u405))

(define-data-var total-savers uint u0)
(define-data-var total-saved uint u0)

(define-map hajj-plans principal {
  goal: uint, saved: uint, target-year: uint, deposits: uint,
  status: (string-ascii 20), started: uint
})
(define-map deposit-log { saver: principal, index: uint } { amount: uint, block: uint })
(define-map deposit-count principal uint)

(define-public (create-plan (goal uint) (target-year uint))
  (begin
    (asserts! (is-none (map-get? hajj-plans tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set hajj-plans tx-sender { goal: goal, saved: u0, target-year: target-year, deposits: u0, status: "saving", started: stacks-block-height })
    (var-set total-savers (+ (var-get total-savers) u1))
    (ok true)))

(define-public (deposit (amount uint))
  (let (
    (plan (unwrap! (map-get? hajj-plans tx-sender) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? deposit-count tx-sender)))
    (new-saved (+ (get saved plan) amount))
  )
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set deposit-log { saver: tx-sender, index: idx } { amount: amount, block: stacks-block-height })
    (map-set deposit-count tx-sender (+ idx u1))
    (map-set hajj-plans tx-sender (merge plan {
      saved: new-saved, deposits: (+ (get deposits plan) u1),
      status: (if (>= new-saved (get goal plan)) "goal-reached" "saving")
    }))
    (var-set total-saved (+ (var-get total-saved) amount))
    (ok new-saved)))

(define-public (withdraw-for-hajj)
  (let ((plan (unwrap! (map-get? hajj-plans tx-sender) ERR-NOT-FOUND)))
    (asserts! (>= (get saved plan) (get goal plan)) ERR-GOAL-NOT-MET)
    (try! (stx-transfer? (get saved plan) CONTRACT-OWNER tx-sender))
    (map-set hajj-plans tx-sender (merge plan { saved: u0, status: "withdrawn" }))
    (ok (get saved plan))))

(define-public (update-goal (new-goal uint))
  (let ((plan (unwrap! (map-get? hajj-plans tx-sender) ERR-NOT-FOUND)))
    (map-set hajj-plans tx-sender (merge plan { goal: new-goal })) (ok new-goal)))

(define-read-only (get-plan (who principal)) (map-get? hajj-plans who))
(define-read-only (get-deposit (saver principal) (index uint)) (map-get? deposit-log { saver: saver, index: index }))
(define-read-only (get-total-savers) (ok (var-get total-savers)))
(define-read-only (get-total-saved) (ok (var-get total-saved)))
(define-read-only (get-progress (who principal))
  (match (map-get? hajj-plans who) p (ok (/ (* (get saved p) u100) (get goal p))) (ok u0)))
