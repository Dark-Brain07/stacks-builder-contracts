;; Community Poll Contract
;; Quick community polls and surveys
;; Halal - consultation (shura)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-VOTED (err u405))
(define-constant ERR-ENDED (err u406))
(define-constant ERR-INVALID-OPTION (err u407))

(define-data-var poll-count uint u0)
(define-data-var total-votes uint u0)

(define-map polls uint {
  creator: principal, question: (string-utf8 200), options-count: uint,
  total-votes: uint, end-block: uint, status: (string-ascii 20), created: uint
})
(define-map poll-options { poll-id: uint, option: uint } { label: (string-utf8 100), votes: uint })
(define-map voter-choices { poll-id: uint, voter: principal } uint)

(define-public (create-poll (question (string-utf8 200)) (option1 (string-utf8 100)) (option2 (string-utf8 100)) (option3 (string-utf8 100)) (duration uint))
  (let ((id (+ (var-get poll-count) u1)))
    (map-set polls id { creator: tx-sender, question: question, options-count: u3, total-votes: u0, end-block: (+ stacks-block-height duration), status: "active", created: stacks-block-height })
    (map-set poll-options { poll-id: id, option: u1 } { label: option1, votes: u0 })
    (map-set poll-options { poll-id: id, option: u2 } { label: option2, votes: u0 })
    (map-set poll-options { poll-id: id, option: u3 } { label: option3, votes: u0 })
    (var-set poll-count id) (ok id)))

(define-public (vote (poll-id uint) (option uint))
  (let (
    (poll (unwrap! (map-get? polls poll-id) ERR-NOT-FOUND))
    (opt (unwrap! (map-get? poll-options { poll-id: poll-id, option: option }) ERR-INVALID-OPTION))
  )
    (asserts! (< stacks-block-height (get end-block poll)) ERR-ENDED)
    (asserts! (is-none (map-get? voter-choices { poll-id: poll-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    (map-set voter-choices { poll-id: poll-id, voter: tx-sender } option)
    (map-set poll-options { poll-id: poll-id, option: option } (merge opt { votes: (+ (get votes opt) u1) }))
    (map-set polls poll-id (merge poll { total-votes: (+ (get total-votes poll) u1) }))
    (var-set total-votes (+ (var-get total-votes) u1))
    (ok option)))

(define-public (close-poll (poll-id uint))
  (let ((poll (unwrap! (map-get? polls poll-id) ERR-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender (get creator poll)) (is-eq tx-sender CONTRACT-OWNER)) ERR-NOT-AUTHORIZED)
    (map-set polls poll-id (merge poll { status: "closed" })) (ok true)))

(define-read-only (get-poll (id uint)) (map-get? polls id))
(define-read-only (get-option (poll-id uint) (option uint)) (map-get? poll-options { poll-id: poll-id, option: option }))
(define-read-only (get-vote (poll-id uint) (voter principal)) (map-get? voter-choices { poll-id: poll-id, voter: voter }))
(define-read-only (get-poll-count) (ok (var-get poll-count)))
(define-read-only (get-total-votes) (ok (var-get total-votes)))
