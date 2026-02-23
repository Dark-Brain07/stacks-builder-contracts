;; Fitness Challenge Contract
;; Community fitness challenges and achievements
;; Halal - healthy body (amanah)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-JOINED (err u405))

(define-data-var challenge-count uint u0)
(define-data-var total-participants uint u0)

(define-map challenges uint {
  creator: principal, name: (string-utf8 100), description: (string-utf8 200),
  goal-type: (string-ascii 20), goal-value: uint,
  participants: uint, completions: uint, reward-pool: uint,
  end-block: uint, status: (string-ascii 20)
})
(define-map participants { challenge-id: uint, participant: principal } { progress: uint, completed: bool, joined: uint })
(define-map progress-log { challenge-id: uint, participant: principal, index: uint } { value: uint, block: uint })
(define-map log-count { challenge-id: uint, participant: principal } uint)

(define-public (create-challenge (name (string-utf8 100)) (description (string-utf8 200)) (goal-type (string-ascii 20)) (goal-value uint) (duration uint) (reward uint))
  (let ((id (+ (var-get challenge-count) u1)))
    (if (> reward u0) (try! (stx-transfer? reward tx-sender CONTRACT-OWNER)) true)
    (map-set challenges id { creator: tx-sender, name: name, description: description, goal-type: goal-type, goal-value: goal-value, participants: u0, completions: u0, reward-pool: reward, end-block: (+ stacks-block-height duration), status: "active" })
    (var-set challenge-count id) (ok id)))

(define-public (join-challenge (challenge-id uint))
  (let ((ch (unwrap! (map-get? challenges challenge-id) ERR-NOT-FOUND)))
    (asserts! (is-none (map-get? participants { challenge-id: challenge-id, participant: tx-sender })) ERR-ALREADY-JOINED)
    (map-set participants { challenge-id: challenge-id, participant: tx-sender } { progress: u0, completed: false, joined: stacks-block-height })
    (map-set log-count { challenge-id: challenge-id, participant: tx-sender } u0)
    (map-set challenges challenge-id (merge ch { participants: (+ (get participants ch) u1) }))
    (var-set total-participants (+ (var-get total-participants) u1)) (ok true)))

(define-public (log-progress (challenge-id uint) (value uint))
  (let (
    (p (unwrap! (map-get? participants { challenge-id: challenge-id, participant: tx-sender }) ERR-NOT-FOUND))
    (ch (unwrap! (map-get? challenges challenge-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? log-count { challenge-id: challenge-id, participant: tx-sender })))
    (new-progress (+ (get progress p) value))
  )
    (map-set progress-log { challenge-id: challenge-id, participant: tx-sender, index: idx } { value: value, block: stacks-block-height })
    (map-set log-count { challenge-id: challenge-id, participant: tx-sender } (+ idx u1))
    (if (and (>= new-progress (get goal-value ch)) (not (get completed p)))
      (begin
        (map-set participants { challenge-id: challenge-id, participant: tx-sender } { progress: new-progress, completed: true, joined: (get joined p) })
        (map-set challenges challenge-id (merge ch { completions: (+ (get completions ch) u1) })))
      (map-set participants { challenge-id: challenge-id, participant: tx-sender } (merge p { progress: new-progress })))
    (ok new-progress)))

(define-read-only (get-challenge (id uint)) (map-get? challenges id))
(define-read-only (get-participant (challenge-id uint) (who principal)) (map-get? participants { challenge-id: challenge-id, participant: who }))
(define-read-only (get-challenge-count) (ok (var-get challenge-count)))
(define-read-only (get-total-participants) (ok (var-get total-participants)))
