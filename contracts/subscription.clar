;; Subscription Service Contract
;; On-chain subscription management with payments
;; Built by rajuice for Stacks Builder Rewards
;; Clarity 4 compatible (no as-contract)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-PLAN-NOT-FOUND (err u404))
(define-constant ERR-SUB-NOT-FOUND (err u405))
(define-constant ERR-ALREADY-SUBSCRIBED (err u406))

(define-data-var plan-count uint u0)
(define-data-var total-revenue uint u0)

(define-map plans uint {
  name: (string-utf8 50),
  price: uint,
  duration: uint,
  active: bool
})

(define-map subscriptions { user: principal, plan-id: uint } {
  start-height: uint,
  end-height: uint,
  auto-renew: bool
})

(define-public (create-plan (name (string-utf8 50)) (price uint) (duration uint))
  (let ((plan-id (+ (var-get plan-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set plans plan-id { name: name, price: price, duration: duration, active: true })
    (var-set plan-count plan-id)
    (ok plan-id)))

(define-public (subscribe (plan-id uint))
  (let ((plan (unwrap! (map-get? plans plan-id) ERR-PLAN-NOT-FOUND)))
    (asserts! (get active plan) ERR-PLAN-NOT-FOUND)
    (asserts! (is-none (map-get? subscriptions { user: tx-sender, plan-id: plan-id })) ERR-ALREADY-SUBSCRIBED)
    (try! (stx-transfer? (get price plan) tx-sender CONTRACT-OWNER))
    (map-set subscriptions { user: tx-sender, plan-id: plan-id } {
      start-height: stacks-block-height,
      end-height: (+ stacks-block-height (get duration plan)),
      auto-renew: false
    })
    (var-set total-revenue (+ (var-get total-revenue) (get price plan)))
    (ok true)))

(define-public (renew-subscription (plan-id uint))
  (let (
    (plan (unwrap! (map-get? plans plan-id) ERR-PLAN-NOT-FOUND))
    (sub (unwrap! (map-get? subscriptions { user: tx-sender, plan-id: plan-id }) ERR-SUB-NOT-FOUND))
  )
    (try! (stx-transfer? (get price plan) tx-sender CONTRACT-OWNER))
    (map-set subscriptions { user: tx-sender, plan-id: plan-id }
      (merge sub { end-height: (+ (get end-height sub) (get duration plan)) }))
    (var-set total-revenue (+ (var-get total-revenue) (get price plan)))
    (ok true)))

(define-public (cancel-subscription (plan-id uint))
  (begin
    (asserts! (is-some (map-get? subscriptions { user: tx-sender, plan-id: plan-id })) ERR-SUB-NOT-FOUND)
    (map-delete subscriptions { user: tx-sender, plan-id: plan-id })
    (ok true)))

(define-public (toggle-plan (plan-id uint))
  (let ((plan (unwrap! (map-get? plans plan-id) ERR-PLAN-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set plans plan-id (merge plan { active: (not (get active plan)) }))
    (ok true)))

(define-read-only (get-plan (plan-id uint))
  (map-get? plans plan-id))

(define-read-only (get-subscription (user principal) (plan-id uint))
  (map-get? subscriptions { user: user, plan-id: plan-id }))

(define-read-only (is-active-subscriber (user principal) (plan-id uint))
  (let ((sub (map-get? subscriptions { user: user, plan-id: plan-id })))
    (match sub
      s (ok (>= (get end-height s) stacks-block-height))
      (ok false))))

(define-read-only (get-plan-count)
  (ok (var-get plan-count)))

(define-read-only (get-total-revenue)
  (ok (var-get total-revenue)))
