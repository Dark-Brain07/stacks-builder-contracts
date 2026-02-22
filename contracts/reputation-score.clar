;; Reputation Score Contract
;; On-chain reputation based on actions
;; Halal - accountability
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-SELF-RATE (err u405))

(define-data-var evaluator-count uint u0)

(define-map evaluators principal bool)
(define-map reputation principal { score: uint, positive: uint, negative: uint, total-actions: uint, last-updated: uint })
(define-map rep-history { user: principal, index: uint } { evaluator: principal, change: int, reason: (string-utf8 100), block: uint })
(define-map history-count principal uint)

(map-set evaluators CONTRACT-OWNER true)

(define-public (add-evaluator (eval principal))
  (begin (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED) (map-set evaluators eval true)
    (var-set evaluator-count (+ (var-get evaluator-count) u1)) (ok true)))

(define-public (add-positive (user principal) (points uint) (reason (string-utf8 100)))
  (let (
    (rep (default-to { score: u100, positive: u0, negative: u0, total-actions: u0, last-updated: u0 } (map-get? reputation user)))
    (idx (default-to u0 (map-get? history-count user)))
  )
    (asserts! (default-to false (map-get? evaluators tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-eq tx-sender user)) ERR-SELF-RATE)
    (map-set reputation user (merge rep { score: (+ (get score rep) points), positive: (+ (get positive rep) u1), total-actions: (+ (get total-actions rep) u1), last-updated: stacks-block-height }))
    (map-set rep-history { user: user, index: idx } { evaluator: tx-sender, change: (to-int points), reason: reason, block: stacks-block-height })
    (map-set history-count user (+ idx u1)) (ok true)))

(define-public (add-negative (user principal) (points uint) (reason (string-utf8 100)))
  (let (
    (rep (default-to { score: u100, positive: u0, negative: u0, total-actions: u0, last-updated: u0 } (map-get? reputation user)))
    (idx (default-to u0 (map-get? history-count user)))
    (new-score (if (>= (get score rep) points) (- (get score rep) points) u0))
  )
    (asserts! (default-to false (map-get? evaluators tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-eq tx-sender user)) ERR-SELF-RATE)
    (map-set reputation user (merge rep { score: new-score, negative: (+ (get negative rep) u1), total-actions: (+ (get total-actions rep) u1), last-updated: stacks-block-height }))
    (map-set rep-history { user: user, index: idx } { evaluator: tx-sender, change: (* (to-int points) (- 1)), reason: reason, block: stacks-block-height })
    (map-set history-count user (+ idx u1)) (ok true)))

(define-read-only (get-reputation (user principal)) (map-get? reputation user))
(define-read-only (get-score (user principal)) (ok (get score (default-to { score: u100, positive: u0, negative: u0, total-actions: u0, last-updated: u0 } (map-get? reputation user)))))
(define-read-only (get-history (user principal) (index uint)) (map-get? rep-history { user: user, index: index }))
(define-read-only (get-history-count (user principal)) (ok (default-to u0 (map-get? history-count user))))
