;; Bounty Board Contract
;; Post and complete bounties/tasks for rewards
;; Halal - fair work compensation
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-DONE (err u405))
(define-constant ERR-SELF-CLAIM (err u406))

(define-data-var bounty-count uint u0)
(define-data-var total-paid uint u0)

(define-map bounties uint {
  creator: principal, title: (string-utf8 100), reward: uint,
  status: (string-ascii 20), hunter: (optional principal), created: uint
})

(define-public (create-bounty (title (string-utf8 100)) (reward uint))
  (let ((id (+ (var-get bounty-count) u1)))
    (map-set bounties id { creator: tx-sender, title: title, reward: reward, status: "open", hunter: none, created: stacks-block-height })
    (var-set bounty-count id) (ok id)))

(define-public (claim-bounty (id uint))
  (let ((bounty (unwrap! (map-get? bounties id) ERR-NOT-FOUND)))
    (asserts! (is-eq (get status bounty) "open") ERR-ALREADY-DONE)
    (asserts! (not (is-eq tx-sender (get creator bounty))) ERR-SELF-CLAIM)
    (map-set bounties id (merge bounty { status: "claimed", hunter: (some tx-sender) })) (ok true)))

(define-public (approve-bounty (id uint))
  (let ((bounty (unwrap! (map-get? bounties id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get creator bounty)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status bounty) "claimed") ERR-ALREADY-DONE)
    (match (get hunter bounty) h
      (begin
        (try! (stx-transfer? (get reward bounty) tx-sender h))
        (map-set bounties id (merge bounty { status: "completed" }))
        (var-set total-paid (+ (var-get total-paid) (get reward bounty)))
        (ok true))
      ERR-NOT-FOUND)))

(define-public (cancel-bounty (id uint))
  (let ((bounty (unwrap! (map-get? bounties id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get creator bounty)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status bounty) "open") ERR-ALREADY-DONE)
    (map-set bounties id (merge bounty { status: "cancelled" })) (ok true)))

(define-read-only (get-bounty (id uint)) (map-get? bounties id))
(define-read-only (get-bounty-count) (ok (var-get bounty-count)))
(define-read-only (get-total-paid) (ok (var-get total-paid)))
