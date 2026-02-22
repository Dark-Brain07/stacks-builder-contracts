;; Crowd Insurance Contract
;; Peer-to-peer cooperative insurance (takaful-style)
;; Halal - mutual risk sharing
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-MEMBER (err u405))
(define-constant ERR-INSUFFICIENT (err u406))

(define-data-var pool-balance uint u0)
(define-data-var member-count uint u0)
(define-data-var claim-count uint u0)
(define-data-var total-paid-out uint u0)

(define-map members principal { premium-paid: uint, coverage: uint, claims-made: uint, joined: uint, active: bool })
(define-map claims uint { claimant: principal, amount: uint, reason: (string-utf8 200), approvals: uint, status: (string-ascii 20), filed: uint })
(define-map claim-votes { claim-id: uint, voter: principal } bool)

(define-public (join-insurance (premium uint) (coverage uint))
  (begin
    (asserts! (is-none (map-get? members tx-sender)) ERR-ALREADY-MEMBER)
    (try! (stx-transfer? premium tx-sender CONTRACT-OWNER))
    (map-set members tx-sender { premium-paid: premium, coverage: coverage, claims-made: u0, joined: stacks-block-height, active: true })
    (var-set pool-balance (+ (var-get pool-balance) premium))
    (var-set member-count (+ (var-get member-count) u1))
    (ok true)))

(define-public (pay-premium (amount uint))
  (let ((m (unwrap! (map-get? members tx-sender) ERR-NOT-FOUND)))
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set members tx-sender (merge m { premium-paid: (+ (get premium-paid m) amount) }))
    (var-set pool-balance (+ (var-get pool-balance) amount))
    (ok amount)))

(define-public (file-claim (amount uint) (reason (string-utf8 200)))
  (let (
    (m (unwrap! (map-get? members tx-sender) ERR-NOT-FOUND))
    (cid (+ (var-get claim-count) u1))
  )
    (asserts! (get active m) ERR-NOT-FOUND)
    (asserts! (<= amount (get coverage m)) ERR-INSUFFICIENT)
    (map-set claims cid { claimant: tx-sender, amount: amount, reason: reason, approvals: u0, status: "pending", filed: stacks-block-height })
    (map-set members tx-sender (merge m { claims-made: (+ (get claims-made m) u1) }))
    (var-set claim-count cid) (ok cid)))

(define-public (vote-claim (claim-id uint) (approve bool))
  (let ((claim (unwrap! (map-get? claims claim-id) ERR-NOT-FOUND)))
    (asserts! (is-some (map-get? members tx-sender)) ERR-NOT-FOUND)
    (asserts! (is-none (map-get? claim-votes { claim-id: claim-id, voter: tx-sender })) ERR-ALREADY-MEMBER)
    (map-set claim-votes { claim-id: claim-id, voter: tx-sender } approve)
    (if approve
      (map-set claims claim-id (merge claim { approvals: (+ (get approvals claim) u1) })) true)
    (ok approve)))

(define-public (process-claim (claim-id uint))
  (let ((claim (unwrap! (map-get? claims claim-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (>= (get approvals claim) u3) ERR-NOT-FOUND)
    (asserts! (>= (var-get pool-balance) (get amount claim)) ERR-INSUFFICIENT)
    (try! (stx-transfer? (get amount claim) CONTRACT-OWNER (get claimant claim)))
    (map-set claims claim-id (merge claim { status: "paid" }))
    (var-set pool-balance (- (var-get pool-balance) (get amount claim)))
    (var-set total-paid-out (+ (var-get total-paid-out) (get amount claim)))
    (ok (get amount claim))))

(define-read-only (get-member (who principal)) (map-get? members who))
(define-read-only (get-claim (id uint)) (map-get? claims id))
(define-read-only (get-pool-balance) (ok (var-get pool-balance)))
(define-read-only (get-member-count) (ok (var-get member-count)))
(define-read-only (get-total-paid-out) (ok (var-get total-paid-out)))
