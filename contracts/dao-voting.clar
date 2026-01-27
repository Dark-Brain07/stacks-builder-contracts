;; DAO Voting Contract
;; Decentralized governance for proposals and voting
;; Built by rajuice for Stacks Builder Rewards

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-VOTED (err u405))
(define-constant ERR-VOTING-CLOSED (err u406))
(define-constant ERR-PROPOSAL-ACTIVE (err u407))
(define-constant ERR-INSUFFICIENT-VOTES (err u408))
(define-constant ERR-NOT-MEMBER (err u409))
(define-constant ERR-PROPOSAL-EXECUTED (err u410))
(define-constant ERR-ALREADY-MEMBER (err u411))

;; Voting parameters
(define-constant VOTING-PERIOD u1008) ;; ~7 days in blocks
(define-constant MIN-VOTES-TO-PASS u3) ;; Minimum votes to pass

;; Data vars
(define-data-var proposal-count uint u0)
(define-data-var member-count uint u0)
(define-data-var treasury-balance uint u0)

;; Proposal structure
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

;; Vote tracking
(define-map votes { proposal-id: uint, voter: principal } {
  vote: bool,
  voting-power: uint
})

;; Member tracking
(define-map members principal {
  voting-power: uint,
  proposals-created: uint,
  total-votes: uint,
  joined-at: uint
})

;; Private function to get contract principal
(define-private (get-contract-principal)
  (as-contract tx-sender))

;; Private function for withdrawing funds
(define-private (withdraw-stx (amount uint) (recipient principal))
  (as-contract (stx-transfer? amount tx-sender recipient)))

;; Proposal Functions

;; Create a new proposal
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
    ;; Update member stats
    (map-set members tx-sender {
      voting-power: (get voting-power member-info),
      proposals-created: (+ (get proposals-created member-info) u1),
      total-votes: (get total-votes member-info),
      joined-at: (get joined-at member-info)
    })
    (var-set proposal-count proposal-id)
    (ok proposal-id)))

;; Vote on a proposal
(define-public (vote (proposal-id uint) (vote-for bool))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
    (member-info (unwrap! (map-get? members tx-sender) ERR-NOT-MEMBER))
    (voting-power (get voting-power member-info))
  )
    ;; Check if voting is still open
    (asserts! (<= stacks-block-height (get end-block proposal)) ERR-VOTING-CLOSED)
    ;; Check if already voted
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    ;; Record the vote
    (map-set votes { proposal-id: proposal-id, voter: tx-sender } {
      vote: vote-for,
      voting-power: voting-power
    })
    ;; Update proposal vote count
    (map-set proposals proposal-id {
      proposer: (get proposer proposal),
      title: (get title proposal),
      description: (get description proposal),
      start-block: (get start-block proposal),
      end-block: (get end-block proposal),
      votes-for: (if vote-for 
        (+ (get votes-for proposal) voting-power) 
        (get votes-for proposal)),
      votes-against: (if (not vote-for) 
        (+ (get votes-against proposal) voting-power) 
        (get votes-against proposal)),
      executed: (get executed proposal)
    })
    ;; Update member stats
    (map-set members tx-sender {
      voting-power: voting-power,
      proposals-created: (get proposals-created member-info),
      total-votes: (+ (get total-votes member-info) u1),
      joined-at: (get joined-at member-info)
    })
    (ok true)))

;; Execute a passed proposal
(define-public (execute-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND)))
    ;; Check voting has ended
    (asserts! (> stacks-block-height (get end-block proposal)) ERR-PROPOSAL-ACTIVE)
    ;; Check not already executed
    (asserts! (not (get executed proposal)) ERR-PROPOSAL-EXECUTED)
    ;; Check it passed
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR-INSUFFICIENT-VOTES)
    (asserts! (>= (+ (get votes-for proposal) (get votes-against proposal)) MIN-VOTES-TO-PASS) ERR-INSUFFICIENT-VOTES)
    ;; Mark as executed
    (map-set proposals proposal-id {
      proposer: (get proposer proposal),
      title: (get title proposal),
      description: (get description proposal),
      start-block: (get start-block proposal),
      end-block: (get end-block proposal),
      votes-for: (get votes-for proposal),
      votes-against: (get votes-against proposal),
      executed: true
    })
    (print { event: "proposal-executed", proposal-id: proposal-id })
    (ok true)))

;; Member Functions

;; Join the DAO (stake STX to become a member)
(define-public (join-dao (stake-amount uint))
  (let ((contract-address (get-contract-principal)))
    (asserts! (>= stake-amount u1000000) ERR-INSUFFICIENT-VOTES) ;; Min 1 STX
    (asserts! (is-none (map-get? members tx-sender)) ERR-ALREADY-MEMBER)
    ;; Transfer stake to DAO treasury
    (try! (stx-transfer? stake-amount tx-sender contract-address))
    ;; Add as member
    (map-set members tx-sender {
      voting-power: (/ stake-amount u1000000), ;; 1 voting power per STX
      proposals-created: u0,
      total-votes: u0,
      joined-at: stacks-block-height
    })
    (var-set member-count (+ (var-get member-count) u1))
    (var-set treasury-balance (+ (var-get treasury-balance) stake-amount))
    (ok (/ stake-amount u1000000))))

;; Leave the DAO
(define-public (leave-dao)
  (let (
    (member-info (unwrap! (map-get? members tx-sender) ERR-NOT-MEMBER))
    (stake-amount (* (get voting-power member-info) u1000000))
    (caller tx-sender)
  )
    ;; Return stake
    (try! (withdraw-stx stake-amount caller))
    ;; Remove member
    (map-delete members caller)
    (var-set member-count (- (var-get member-count) u1))
    (var-set treasury-balance (- (var-get treasury-balance) stake-amount))
    (ok stake-amount)))

;; Increase stake
(define-public (increase-stake (additional-amount uint))
  (let (
    (member-info (unwrap! (map-get? members tx-sender) ERR-NOT-MEMBER))
    (contract-address (get-contract-principal))
  )
    (try! (stx-transfer? additional-amount tx-sender contract-address))
    (map-set members tx-sender {
      voting-power: (+ (get voting-power member-info) (/ additional-amount u1000000)),
      proposals-created: (get proposals-created member-info),
      total-votes: (get total-votes member-info),
      joined-at: (get joined-at member-info)
    })
    (var-set treasury-balance (+ (var-get treasury-balance) additional-amount))
    (ok (+ (get voting-power member-info) (/ additional-amount u1000000)))))

;; Admin Functions

;; Add member directly (owner only - for initial setup)
(define-public (add-member (member principal) (voting-power uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set members member {
      voting-power: voting-power,
      proposals-created: u0,
      total-votes: u0,
      joined-at: stacks-block-height
    })
    (var-set member-count (+ (var-get member-count) u1))
    (ok true)))

;; Read-only Functions

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id))

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter }))

(define-read-only (get-member (member principal))
  (map-get? members member))

(define-read-only (get-proposal-count)
  (ok (var-get proposal-count)))

(define-read-only (get-member-count)
  (ok (var-get member-count)))

(define-read-only (get-treasury-balance)
  (ok (var-get treasury-balance)))

(define-read-only (get-voting-period)
  (ok VOTING-PERIOD))

(define-read-only (get-min-votes)
  (ok MIN-VOTES-TO-PASS))

(define-read-only (is-proposal-active (proposal-id uint))
  (let ((proposal (map-get? proposals proposal-id)))
    (match proposal
      p (ok (and (<= stacks-block-height (get end-block p)) (not (get executed p))))
      (err ERR-PROPOSAL-NOT-FOUND))))

(define-read-only (is-member (account principal))
  (is-some (map-get? members account)))
