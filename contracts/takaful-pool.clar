;; Takaful Pool Contract
;; Islamic mutual insurance - halal cooperative risk-sharing
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-ALREADY-MEMBER (err u402))
(define-constant ERR-NOT-MEMBER (err u403))
(define-constant ERR-CLAIM-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT-POOL (err u405))
(define-constant MIN-CONTRIBUTION u1000000) ;; 1 STX

(define-data-var pool-balance uint u0)
(define-data-var member-count uint u0)
(define-data-var claim-count uint u0)
(define-data-var total-claims-paid uint u0)

(define-map pool-members principal { contribution: uint, joined: uint, claims-made: uint })
(define-map claims uint { claimant: principal, amount: uint, reason: (string-utf8 200), approved: bool, paid: bool, created: uint })

(define-public (join-pool (contribution uint))
  (begin
    (asserts! (>= contribution MIN-CONTRIBUTION) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? pool-members tx-sender)) ERR-ALREADY-MEMBER)
    (try! (stx-transfer? contribution tx-sender CONTRACT-OWNER))
    (map-set pool-members tx-sender { contribution: contribution, joined: stacks-block-height, claims-made: u0 })
    (var-set pool-balance (+ (var-get pool-balance) contribution))
    (var-set member-count (+ (var-get member-count) u1))
    (ok true)))

(define-public (add-contribution (amount uint))
  (let ((member (unwrap! (map-get? pool-members tx-sender) ERR-NOT-MEMBER)))
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set pool-members tx-sender (merge member { contribution: (+ (get contribution member) amount) }))
    (var-set pool-balance (+ (var-get pool-balance) amount))
    (ok true)))

(define-public (submit-claim (amount uint) (reason (string-utf8 200)))
  (let (
    (member (unwrap! (map-get? pool-members tx-sender) ERR-NOT-MEMBER))
    (id (+ (var-get claim-count) u1))
  )
    (map-set claims id { claimant: tx-sender, amount: amount, reason: reason, approved: false, paid: false, created: stacks-block-height })
    (map-set pool-members tx-sender (merge member { claims-made: (+ (get claims-made member) u1) }))
    (var-set claim-count id)
    (ok id)))

(define-public (approve-claim (claim-id uint))
  (let ((claim (unwrap! (map-get? claims claim-id) ERR-CLAIM-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set claims claim-id (merge claim { approved: true }))
    (ok true)))

(define-public (pay-claim (claim-id uint))
  (let ((claim (unwrap! (map-get? claims claim-id) ERR-CLAIM-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (get approved claim) ERR-NOT-AUTHORIZED)
    (asserts! (>= (var-get pool-balance) (get amount claim)) ERR-INSUFFICIENT-POOL)
    (try! (stx-transfer? (get amount claim) tx-sender (get claimant claim)))
    (map-set claims claim-id (merge claim { paid: true }))
    (var-set pool-balance (- (var-get pool-balance) (get amount claim)))
    (var-set total-claims-paid (+ (var-get total-claims-paid) (get amount claim)))
    (ok true)))

(define-read-only (get-member (who principal)) (map-get? pool-members who))
(define-read-only (get-claim (id uint)) (map-get? claims id))
(define-read-only (get-pool-balance) (ok (var-get pool-balance)))
(define-read-only (get-member-count) (ok (var-get member-count)))
(define-read-only (get-claim-count) (ok (var-get claim-count)))
(define-read-only (get-total-claims-paid) (ok (var-get total-claims-paid)))
