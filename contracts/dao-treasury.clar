;; DAO Treasury Contract
;; Decentralized treasury management
;; Halal - transparent community finance
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-VOTED (err u405))
(define-constant ERR-THRESHOLD-NOT-MET (err u406))
(define-constant APPROVAL-THRESHOLD u60) ;; 60% required

(define-data-var treasury-balance uint u0)
(define-data-var spending-count uint u0)
(define-data-var member-count uint u0)

(define-map members principal { weight: uint, joined: uint })
(define-map spending-proposals uint {
  proposer: principal, recipient: principal, amount: uint,
  reason: (string-utf8 200), yes-votes: uint, no-votes: uint,
  total-voters: uint, status: (string-ascii 20), created: uint
})
(define-map proposal-votes { proposal-id: uint, voter: principal } bool)

(define-public (add-member (member principal) (weight uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set members member { weight: weight, joined: stacks-block-height })
    (var-set member-count (+ (var-get member-count) u1)) (ok true)))

(define-public (deposit (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (var-set treasury-balance (+ (var-get treasury-balance) amount)) (ok amount)))

(define-public (propose-spending (recipient principal) (amount uint) (reason (string-utf8 200)))
  (let ((id (+ (var-get spending-count) u1)))
    (asserts! (is-some (map-get? members tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set spending-proposals id { proposer: tx-sender, recipient: recipient, amount: amount, reason: reason, yes-votes: u0, no-votes: u0, total-voters: u0, status: "voting", created: stacks-block-height })
    (var-set spending-count id) (ok id)))

(define-public (vote-spending (proposal-id uint) (support bool))
  (let (
    (proposal (unwrap! (map-get? spending-proposals proposal-id) ERR-NOT-FOUND))
    (member (unwrap! (map-get? members tx-sender) ERR-NOT-AUTHORIZED))
  )
    (asserts! (is-eq (get status proposal) "voting") ERR-NOT-FOUND)
    (asserts! (is-none (map-get? proposal-votes { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    (map-set proposal-votes { proposal-id: proposal-id, voter: tx-sender } true)
    (if support
      (map-set spending-proposals proposal-id (merge proposal { yes-votes: (+ (get yes-votes proposal) (get weight member)), total-voters: (+ (get total-voters proposal) u1) }))
      (map-set spending-proposals proposal-id (merge proposal { no-votes: (+ (get no-votes proposal) (get weight member)), total-voters: (+ (get total-voters proposal) u1) })))
    (ok true)))

(define-public (execute-spending (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? spending-proposals proposal-id) ERR-NOT-FOUND))
    (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
    (approval-pct (if (> total-votes u0) (/ (* (get yes-votes proposal) u100) total-votes) u0))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (>= approval-pct APPROVAL-THRESHOLD) ERR-THRESHOLD-NOT-MET)
    (try! (stx-transfer? (get amount proposal) tx-sender (get recipient proposal)))
    (map-set spending-proposals proposal-id (merge proposal { status: "executed" }))
    (var-set treasury-balance (- (var-get treasury-balance) (get amount proposal)))
    (ok (get amount proposal))))

(define-read-only (get-proposal (id uint)) (map-get? spending-proposals id))
(define-read-only (get-member (who principal)) (map-get? members who))
(define-read-only (get-treasury-balance) (ok (var-get treasury-balance)))
(define-read-only (get-spending-count) (ok (var-get spending-count)))
(define-read-only (get-member-count) (ok (var-get member-count)))
