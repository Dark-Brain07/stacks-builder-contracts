;; Profit Pool Contract
;; Shared revenue pool with proportional distribution
;; Halal - mudarabah/musharakah style
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT (err u405))

(define-data-var pool-balance uint u0)
(define-data-var total-deposits uint u0)
(define-data-var total-distributed uint u0)
(define-data-var member-count uint u0)
(define-data-var distribution-round uint u0)

(define-map pool-members principal { deposit: uint, share-pct: uint, claimed: uint, joined: uint })
(define-map distributions uint { total: uint, per-share: uint, block: uint })

(define-public (join-pool (deposit uint))
  (begin
    (asserts! (is-none (map-get? pool-members tx-sender)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? deposit tx-sender CONTRACT-OWNER))
    (map-set pool-members tx-sender { deposit: deposit, share-pct: u0, claimed: u0, joined: stacks-block-height })
    (var-set pool-balance (+ (var-get pool-balance) deposit))
    (var-set total-deposits (+ (var-get total-deposits) deposit))
    (var-set member-count (+ (var-get member-count) u1))
    (ok true)))

(define-public (add-to-pool (amount uint))
  (let ((member (unwrap! (map-get? pool-members tx-sender) ERR-NOT-FOUND)))
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set pool-members tx-sender (merge member { deposit: (+ (get deposit member) amount) }))
    (var-set pool-balance (+ (var-get pool-balance) amount))
    (var-set total-deposits (+ (var-get total-deposits) amount))
    (ok amount)))

(define-public (set-share (member principal) (share uint))
  (let ((m (unwrap! (map-get? pool-members member) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set pool-members member (merge m { share-pct: share })) (ok share)))

(define-public (add-profit (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set pool-balance (+ (var-get pool-balance) amount)) (ok amount)))

(define-public (distribute-to (member principal) (amount uint))
  (let ((m (unwrap! (map-get? pool-members member) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? amount tx-sender member))
    (map-set pool-members member (merge m { claimed: (+ (get claimed m) amount) }))
    (var-set total-distributed (+ (var-get total-distributed) amount))
    (ok amount)))

(define-public (withdraw (amount uint))
  (let ((member (unwrap! (map-get? pool-members tx-sender) ERR-NOT-FOUND)))
    (asserts! (>= (get deposit member) amount) ERR-INSUFFICIENT)
    (try! (stx-transfer? amount CONTRACT-OWNER tx-sender))
    (map-set pool-members tx-sender (merge member { deposit: (- (get deposit member) amount) }))
    (var-set pool-balance (- (var-get pool-balance) amount))
    (ok amount)))

(define-read-only (get-member (who principal)) (map-get? pool-members who))
(define-read-only (get-pool-balance) (ok (var-get pool-balance)))
(define-read-only (get-total-distributed) (ok (var-get total-distributed)))
(define-read-only (get-member-count) (ok (var-get member-count)))
