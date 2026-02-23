;; Debate Club Contract
;; Community debate competitions
;; Halal - intellectual discourse
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var debate-count uint u0)
(define-data-var total-debates uint u0)

(define-map debates uint { moderator: principal, topic: (string-utf8 200), format: (string-ascii 20), for-speaker: principal, against-speaker: principal, for-votes: uint, against-votes: uint, status: (string-ascii 20), created: uint })
(define-map debate-votes { debate-id: uint, voter: principal } (string-ascii 10))
(define-map speaker-stats principal { debates: uint, wins: uint, total-votes: uint })

(define-public (create-debate (topic (string-utf8 200)) (format (string-ascii 20)) (for-speaker principal) (against-speaker principal))
  (let ((id (+ (var-get debate-count) u1)))
    (map-set debates id { moderator: tx-sender, topic: topic, format: format, for-speaker: for-speaker, against-speaker: against-speaker, for-votes: u0, against-votes: u0, status: "open", created: stacks-block-height })
    (var-set debate-count id) (ok id)))

(define-public (vote (debate-id uint) (side (string-ascii 10)))
  (let ((d (unwrap! (map-get? debates debate-id) ERR-NOT-FOUND)))
    (asserts! (is-eq (get status d) "open") ERR-NOT-FOUND)
    (asserts! (is-none (map-get? debate-votes { debate-id: debate-id, voter: tx-sender })) ERR-NOT-AUTHORIZED)
    (map-set debate-votes { debate-id: debate-id, voter: tx-sender } side)
    (if (is-eq side "for")
      (map-set debates debate-id (merge d { for-votes: (+ (get for-votes d) u1) }))
      (map-set debates debate-id (merge d { against-votes: (+ (get against-votes d) u1) })))
    (ok side)))

(define-public (close-debate (debate-id uint))
  (let (
    (d (unwrap! (map-get? debates debate-id) ERR-NOT-FOUND))
    (winner (if (> (get for-votes d) (get against-votes d)) (get for-speaker d) (get against-speaker d)))
    (ws (default-to { debates: u0, wins: u0, total-votes: u0 } (map-get? speaker-stats winner)))
    (fs (default-to { debates: u0, wins: u0, total-votes: u0 } (map-get? speaker-stats (get for-speaker d))))
    (as (default-to { debates: u0, wins: u0, total-votes: u0 } (map-get? speaker-stats (get against-speaker d))))
  )
    (asserts! (is-eq tx-sender (get moderator d)) ERR-NOT-AUTHORIZED)
    (map-set debates debate-id (merge d { status: "closed" }))
    (map-set speaker-stats winner (merge ws { wins: (+ (get wins ws) u1) }))
    (map-set speaker-stats (get for-speaker d) (merge fs { debates: (+ (get debates fs) u1), total-votes: (+ (get total-votes fs) (get for-votes d)) }))
    (map-set speaker-stats (get against-speaker d) (merge as { debates: (+ (get debates as) u1), total-votes: (+ (get total-votes as) (get against-votes d)) }))
    (var-set total-debates (+ (var-get total-debates) u1)) (ok true)))

(define-read-only (get-debate (id uint)) (map-get? debates id))
(define-read-only (get-vote (debate-id uint) (voter principal)) (map-get? debate-votes { debate-id: debate-id, voter: voter }))
(define-read-only (get-speaker (who principal)) (map-get? speaker-stats who))
(define-read-only (get-debate-count) (ok (var-get debate-count)))
(define-read-only (get-total-debates) (ok (var-get total-debates)))
