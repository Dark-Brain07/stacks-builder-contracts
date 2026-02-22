;; Apprentice Program Contract
;; Apprenticeship tracking and certification
;; Halal - knowledge transfer
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var apprentice-count uint u0)
(define-data-var completion-count uint u0)

(define-map apprenticeships uint {
  apprentice: principal, master: principal, trade: (string-utf8 100),
  hours-required: uint, hours-logged: uint, milestones-done: uint,
  status: (string-ascii 20), started: uint
})
(define-map hour-logs { apprentice-id: uint, index: uint } { hours: uint, task: (string-utf8 100), block: uint })
(define-map log-count uint uint)
(define-map masters principal { name: (string-utf8 100), trade: (string-utf8 100), apprentices: uint })

(define-public (register-master (name (string-utf8 100)) (trade (string-utf8 100)))
  (begin (map-set masters tx-sender { name: name, trade: trade, apprentices: u0 }) (ok true)))

(define-public (create-apprenticeship (apprentice principal) (trade (string-utf8 100)) (hours uint))
  (let (
    (id (+ (var-get apprentice-count) u1))
    (m (unwrap! (map-get? masters tx-sender) ERR-NOT-AUTHORIZED))
  )
    (map-set apprenticeships id { apprentice: apprentice, master: tx-sender, trade: trade, hours-required: hours, hours-logged: u0, milestones-done: u0, status: "active", started: stacks-block-height })
    (map-set masters tx-sender (merge m { apprentices: (+ (get apprentices m) u1) }))
    (var-set apprentice-count id) (ok id)))

(define-public (log-hours (apprentice-id uint) (hours uint) (task (string-utf8 100)))
  (let (
    (app (unwrap! (map-get? apprenticeships apprentice-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? log-count apprentice-id)))
  )
    (asserts! (is-eq tx-sender (get master app)) ERR-NOT-AUTHORIZED)
    (map-set hour-logs { apprentice-id: apprentice-id, index: idx } { hours: hours, task: task, block: stacks-block-height })
    (map-set log-count apprentice-id (+ idx u1))
    (map-set apprenticeships apprentice-id (merge app { hours-logged: (+ (get hours-logged app) hours) }))
    (ok hours)))

(define-public (complete-milestone (apprentice-id uint))
  (let ((app (unwrap! (map-get? apprenticeships apprentice-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get master app)) ERR-NOT-AUTHORIZED)
    (map-set apprenticeships apprentice-id (merge app { milestones-done: (+ (get milestones-done app) u1) })) (ok true)))

(define-public (graduate (apprentice-id uint))
  (let ((app (unwrap! (map-get? apprenticeships apprentice-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get master app)) ERR-NOT-AUTHORIZED)
    (asserts! (>= (get hours-logged app) (get hours-required app)) ERR-NOT-FOUND)
    (map-set apprenticeships apprentice-id (merge app { status: "graduated" }))
    (var-set completion-count (+ (var-get completion-count) u1)) (ok true)))

(define-read-only (get-apprenticeship (id uint)) (map-get? apprenticeships id))
(define-read-only (get-hour-log (id uint) (index uint)) (map-get? hour-logs { apprentice-id: id, index: index }))
(define-read-only (get-master (who principal)) (map-get? masters who))
(define-read-only (get-apprentice-count) (ok (var-get apprentice-count)))
(define-read-only (get-completion-count) (ok (var-get completion-count)))
