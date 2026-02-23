;; DAO Voting Contract
;; Decentralized governance with proposals
;; Built by rajuice for Stacks Builder Rewards
;; Clarity 4 compatible (no as-contract)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-MEMBER (err u402))
(define-constant ERR-ALREADY-VOTED (err u403))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u404))
(define-constant ERR-VOTING-CLOSED (err u405))
(define-constant ERR-PROPOSAL-ACTIVE (err u406))
(define-constant ERR-ALREADY-MEMBER (err u407))
(define-constant ERR-PROPOSAL-EXECUTED (err u408))
(define-constant ERR-PROPOSAL-FAILED (err u409))
(define-constant ERR-INVALID-AMOUNT (err u410))

(define-constant VOTING-PERIOD u1008) ;; ~7 days in blocks
(define-constant MIN-STAKE u5000000) ;; 5 STX minimum
(define-constant PASSING-THRESHOLD u51) ;; 51% to pass

(define-data-var proposal-count uint u0)
(define-data-var member-count uint u0)
(define-data-var treasury-balance uint u0)

(define-map proposals uint {
  proposer: principal,
  title: (string-utf8 100),
  description: (string-utf8 500),
  start-block: uint,
  end-block: uint,
  votes-for: uint,
  votes-against: uint,
  executed: bool
})

(define-map votes { proposal-id: uint, voter: principal } { vote-for: bool, power: uint })
(define-map members principal { voting-power: uint, proposals-created: uint, total-votes: uint, joined-at: uint })

;; Create a proposal
(define-public (create-proposal (title (string-utf8 100)) (description (string-utf8 500)))
  (let (
    (member-info (unwrap! (map-get? members tx-sender) ERR-NOT-MEMBER))
    (proposal-id (+ (var-get proposal-count) u1))
  )
    (map-set proposals proposal-id {
      proposer: tx-sender,
      title: title,
      description: description,
      start-block: stacks-block-height,
      end-block: (+ stacks-block-height VOTING-PERIOD),
      votes-for: u0,
      votes-against: u0,
      executed: false
    })
    (map-set members tx-sender (merge member-info { proposals-created: (+ (get proposals-created member-info) u1) }))
    (var-set proposal-count proposal-id)
    (ok proposal-id)))

;; Vote on a proposal
(define-public (vote (proposal-id uint) (vote-for bool))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
    (member-info (unwrap! (map-get? members tx-sender) ERR-NOT-MEMBER))
    (voting-power (get voting-power member-info))
  )
    (asserts! (<= stacks-block-height (get end-block proposal)) ERR-VOTING-CLOSED)
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    (map-set votes { proposal-id: proposal-id, voter: tx-sender } { vote-for: vote-for, power: voting-power })
    (if vote-for
      (map-set proposals proposal-id (merge proposal { votes-for: (+ (get votes-for proposal) voting-power) }))
      (map-set proposals proposal-id (merge proposal { votes-against: (+ (get votes-against proposal) voting-power) })))
    (map-set members tx-sender (merge member-info { total-votes: (+ (get total-votes member-info) u1) }))
    (ok true)))

;; Execute a passed proposal
(define-public (execute-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND)))
    (asserts! (> stacks-block-height (get end-block proposal)) ERR-PROPOSAL-ACTIVE)
    (asserts! (not (get executed proposal)) ERR-PROPOSAL-EXECUTED)
    (let (
      (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
      (pass-pct (if (> total-votes u0) (/ (* (get votes-for proposal) u100) total-votes) u0))
    )
      (asserts! (>= pass-pct PASSING-THRESHOLD) ERR-PROPOSAL-FAILED)
      (map-set proposals proposal-id (merge proposal { executed: true }))
      (print { event: "proposal-executed", id: proposal-id, votes-for: (get votes-for proposal), votes-against: (get votes-against proposal) })
      (ok true))))

;; Join DAO by staking - funds go to owner (treasury manager)
(define-public (join-dao (stake-amount uint))
  (begin
    (asserts! (>= stake-amount MIN-STAKE) ERR-INVALID-AMOUNT)
    (asserts! (is-none (map-get? members tx-sender)) ERR-ALREADY-MEMBER)
    (try! (stx-transfer? stake-amount tx-sender CONTRACT-OWNER))
    (map-set members tx-sender {
      voting-power: (/ stake-amount u1000000),
      proposals-created: u0,
      total-votes: u0,
      joined-at: stacks-block-height
    })
    (var-set member-count (+ (var-get member-count) u1))
    (var-set treasury-balance (+ (var-get treasury-balance) stake-amount))
    (ok true)))

;; Leave DAO
(define-public (leave-dao)
  (begin
    (asserts! (is-some (map-get? members tx-sender)) ERR-NOT-MEMBER)
    (map-delete members tx-sender)
    (var-set member-count (- (var-get member-count) u1))
    (print { event: "member-left", member: tx-sender })
    (ok true)))

;; Owner adds member directly
(define-public (add-member (member principal) (voting-power uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? members member)) ERR-ALREADY-MEMBER)
    (map-set members member {
      voting-power: voting-power,
      proposals-created: u0,
      total-votes: u0,
      joined-at: stacks-block-height
    })
    (var-set member-count (+ (var-get member-count) u1))
    (ok true)))

(define-read-only (get-proposal (proposal-id uint)) (map-get? proposals proposal-id))
(define-read-only (get-member (member principal)) (map-get? members member))
(define-read-only (get-proposal-count) (ok (var-get proposal-count)))
(define-read-only (get-member-count) (ok (var-get member-count)))
(define-read-only (get-treasury-balance) (ok (var-get treasury-balance)))
(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter }))

(define-read-only (is-proposal-active (proposal-id uint))
  (let ((proposal (map-get? proposals proposal-id)))
    (match proposal
      p (ok (and (<= stacks-block-height (get end-block p)) (not (get executed p))))
      (err ERR-PROPOSAL-NOT-FOUND))))

(define-read-only (is-member (account principal))
  (is-some (map-get? members account)))
