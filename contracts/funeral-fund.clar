;; Funeral Fund Contract
;; Janazah funeral expense community fund
;; Halal - caring for deceased (janazah)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var member-count uint u0)
(define-data-var claim-count uint u0)
(define-data-var pool-balance uint u0)

(define-map members principal { name: (string-utf8 100), contributions: uint, joined: uint, active: bool })
(define-map claims uint { claimant: principal, deceased-name: (string-utf8 100), expenses: uint, approved: bool, paid: bool, filed: uint })
(define-map expense-items { claim-id: uint, index: uint } { item: (string-utf8 100), amount: uint })
(define-map item-count uint uint)

(define-public (join-fund (name (string-utf8 100)) (contribution uint))
  (begin
    (try! (stx-transfer? contribution tx-sender CONTRACT-OWNER))
    (map-set members tx-sender { name: name, contributions: contribution, joined: stacks-block-height, active: true })
    (var-set pool-balance (+ (var-get pool-balance) contribution))
    (var-set member-count (+ (var-get member-count) u1)) (ok true)))

(define-public (contribute (amount uint))
  (let ((m (unwrap! (map-get? members tx-sender) ERR-NOT-FOUND)))
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set members tx-sender (merge m { contributions: (+ (get contributions m) amount) }))
    (var-set pool-balance (+ (var-get pool-balance) amount)) (ok amount)))

(define-public (file-claim (deceased-name (string-utf8 100)) (total-expenses uint))
  (let ((id (+ (var-get claim-count) u1)))
    (asserts! (is-some (map-get? members tx-sender)) ERR-NOT-FOUND)
    (map-set claims id { claimant: tx-sender, deceased-name: deceased-name, expenses: total-expenses, approved: false, paid: false, filed: stacks-block-height })
    (var-set claim-count id) (ok id)))

(define-public (add-expense (claim-id uint) (item (string-utf8 100)) (amount uint))
  (let (
    (c (unwrap! (map-get? claims claim-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? item-count claim-id)))
  )
    (asserts! (is-eq tx-sender (get claimant c)) ERR-NOT-AUTHORIZED)
    (map-set expense-items { claim-id: claim-id, index: idx } { item: item, amount: amount })
    (map-set item-count claim-id (+ idx u1)) (ok idx)))

(define-public (approve-and-pay (claim-id uint))
  (let ((c (unwrap! (map-get? claims claim-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (>= (var-get pool-balance) (get expenses c)) ERR-NOT-FOUND)
    (try! (stx-transfer? (get expenses c) CONTRACT-OWNER (get claimant c)))
    (map-set claims claim-id (merge c { approved: true, paid: true }))
    (var-set pool-balance (- (var-get pool-balance) (get expenses c))) (ok (get expenses c))))

(define-read-only (get-member (who principal)) (map-get? members who))
(define-read-only (get-claim (id uint)) (map-get? claims id))
(define-read-only (get-expense (claim-id uint) (index uint)) (map-get? expense-items { claim-id: claim-id, index: index }))
(define-read-only (get-member-count) (ok (var-get member-count)))
(define-read-only (get-pool-balance) (ok (var-get pool-balance)))
