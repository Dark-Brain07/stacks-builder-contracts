;; Quadratic Voting Contract
;; Fair voting where cost increases quadratically
;; Halal - equitable governance
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ENDED (err u405))
(define-constant ERR-INSUFFICIENT-CREDITS (err u406))
(define-constant CREDITS-PER-STX u100)

(define-data-var proposal-count uint u0)
(define-data-var total-credits-purchased uint u0)

(define-map voter-credits principal uint)
(define-map proposals uint { title: (string-utf8 100), creator: principal, votes-for: uint, votes-against: uint, end-block: uint })
(define-map voter-allocations { proposal-id: uint, voter: principal } { votes-for: uint, votes-against: uint })

(define-public (buy-credits (stx-amount uint))
  (let ((credits (* stx-amount CREDITS-PER-STX)))
    (try! (stx-transfer? stx-amount tx-sender CONTRACT-OWNER))
    (map-set voter-credits tx-sender (+ (default-to u0 (map-get? voter-credits tx-sender)) credits))
    (var-set total-credits-purchased (+ (var-get total-credits-purchased) credits))
    (ok credits)))

(define-public (create-proposal (title (string-utf8 100)) (duration uint))
  (let ((id (+ (var-get proposal-count) u1)))
    (map-set proposals id { title: title, creator: tx-sender, votes-for: u0, votes-against: u0, end-block: (+ stacks-block-height duration) })
    (var-set proposal-count id) (ok id)))

(define-public (vote (proposal-id uint) (num-votes uint) (support bool))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) ERR-NOT-FOUND))
    (cost (* num-votes num-votes))
    (balance (default-to u0 (map-get? voter-credits tx-sender)))
    (prev (default-to { votes-for: u0, votes-against: u0 } (map-get? voter-allocations { proposal-id: proposal-id, voter: tx-sender })))
  )
    (asserts! (< stacks-block-height (get end-block proposal)) ERR-ENDED)
    (asserts! (>= balance cost) ERR-INSUFFICIENT-CREDITS)
    (map-set voter-credits tx-sender (- balance cost))
    (if support
      (begin
        (map-set proposals proposal-id (merge proposal { votes-for: (+ (get votes-for proposal) num-votes) }))
        (map-set voter-allocations { proposal-id: proposal-id, voter: tx-sender } (merge prev { votes-for: (+ (get votes-for prev) num-votes) })))
      (begin
        (map-set proposals proposal-id (merge proposal { votes-against: (+ (get votes-against proposal) num-votes) }))
        (map-set voter-allocations { proposal-id: proposal-id, voter: tx-sender } (merge prev { votes-against: (+ (get votes-against prev) num-votes) }))))
    (ok cost)))

(define-read-only (get-proposal (id uint)) (map-get? proposals id))
(define-read-only (get-credits (who principal)) (ok (default-to u0 (map-get? voter-credits who))))
(define-read-only (get-vote-cost (num-votes uint)) (ok (* num-votes num-votes)))
(define-read-only (get-voter-allocation (proposal-id uint) (voter principal)) (map-get? voter-allocations { proposal-id: proposal-id, voter: voter }))
(define-read-only (get-proposal-count) (ok (var-get proposal-count)))
