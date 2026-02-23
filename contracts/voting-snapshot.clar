;; Voting Snapshot Contract
;; Snapshot-based voting with recorded balances
;; Halal - fair governance
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-VOTED (err u405))
(define-constant ERR-ENDED (err u406))
(define-constant ERR-NO-BALANCE (err u407))

(define-data-var snapshot-count uint u0)
(define-data-var proposal-count uint u0)

(define-map snapshots uint { block: uint, total-supply: uint })
(define-map snapshot-balances { snapshot-id: uint, holder: principal } uint)
(define-map proposals uint { title: (string-utf8 100), snapshot-id: uint, yes-votes: uint, no-votes: uint, end-block: uint, status: (string-ascii 20) })
(define-map proposal-votes { proposal-id: uint, voter: principal } bool)

(define-public (create-snapshot)
  (let ((id (+ (var-get snapshot-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set snapshots id { block: stacks-block-height, total-supply: u0 })
    (var-set snapshot-count id) (ok id)))

(define-public (record-balance (snapshot-id uint) (holder principal) (balance uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? snapshots snapshot-id)) ERR-NOT-FOUND)
    (map-set snapshot-balances { snapshot-id: snapshot-id, holder: holder } balance)
    (let ((snap (unwrap-panic (map-get? snapshots snapshot-id))))
      (map-set snapshots snapshot-id (merge snap { total-supply: (+ (get total-supply snap) balance) })))
    (ok true)))

(define-public (create-proposal (title (string-utf8 100)) (snapshot-id uint) (duration uint))
  (let ((id (+ (var-get proposal-count) u1)))
    (asserts! (is-some (map-get? snapshots snapshot-id)) ERR-NOT-FOUND)
    (map-set proposals id { title: title, snapshot-id: snapshot-id, yes-votes: u0, no-votes: u0, end-block: (+ stacks-block-height duration), status: "active" })
    (var-set proposal-count id) (ok id)))

(define-public (vote (proposal-id uint) (support bool))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) ERR-NOT-FOUND))
    (balance (unwrap! (map-get? snapshot-balances { snapshot-id: (get snapshot-id proposal), holder: tx-sender }) ERR-NO-BALANCE))
  )
    (asserts! (< stacks-block-height (get end-block proposal)) ERR-ENDED)
    (asserts! (is-none (map-get? proposal-votes { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    (map-set proposal-votes { proposal-id: proposal-id, voter: tx-sender } true)
    (if support
      (map-set proposals proposal-id (merge proposal { yes-votes: (+ (get yes-votes proposal) balance) }))
      (map-set proposals proposal-id (merge proposal { no-votes: (+ (get no-votes proposal) balance) })))
    (ok balance)))

(define-read-only (get-proposal (id uint)) (map-get? proposals id))
(define-read-only (get-snapshot (id uint)) (map-get? snapshots id))
(define-read-only (get-snapshot-balance (snapshot-id uint) (holder principal)) (ok (default-to u0 (map-get? snapshot-balances { snapshot-id: snapshot-id, holder: holder }))))
(define-read-only (get-proposal-count) (ok (var-get proposal-count)))
