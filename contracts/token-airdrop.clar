;; Token Airdrop Contract
;; Community token airdrop distribution
;; Halal - fair token distribution
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-CLAIMED (err u405))
(define-constant ERR-NOT-ELIGIBLE (err u406))

(define-data-var campaign-count uint u0)
(define-data-var total-distributed uint u0)

(define-map campaigns uint {
  creator: principal, name: (string-utf8 100), amount-per-claim: uint,
  total-budget: uint, claimed-total: uint, claimants: uint,
  max-claimants: uint, status: (string-ascii 20), created: uint
})
(define-map eligible { campaign-id: uint, wallet: principal } bool)
(define-map claimed { campaign-id: uint, wallet: principal } uint)

(define-public (create-campaign (name (string-utf8 100)) (amount-per-claim uint) (budget uint) (max-claimants uint))
  (let ((id (+ (var-get campaign-count) u1)))
    (try! (stx-transfer? budget tx-sender CONTRACT-OWNER))
    (map-set campaigns id { creator: tx-sender, name: name, amount-per-claim: amount-per-claim, total-budget: budget, claimed-total: u0, claimants: u0, max-claimants: max-claimants, status: "active", created: stacks-block-height })
    (var-set campaign-count id) (ok id)))

(define-public (add-eligible (campaign-id uint) (wallet principal))
  (let ((camp (unwrap! (map-get? campaigns campaign-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get creator camp)) ERR-NOT-AUTHORIZED)
    (map-set eligible { campaign-id: campaign-id, wallet: wallet } true)
    (ok true)))

(define-public (claim-airdrop (campaign-id uint))
  (let ((camp (unwrap! (map-get? campaigns campaign-id) ERR-NOT-FOUND)))
    (asserts! (is-eq (get status camp) "active") ERR-NOT-FOUND)
    (asserts! (default-to false (map-get? eligible { campaign-id: campaign-id, wallet: tx-sender })) ERR-NOT-ELIGIBLE)
    (asserts! (is-none (map-get? claimed { campaign-id: campaign-id, wallet: tx-sender })) ERR-ALREADY-CLAIMED)
    (asserts! (< (get claimants camp) (get max-claimants camp)) ERR-NOT-FOUND)
    (try! (stx-transfer? (get amount-per-claim camp) CONTRACT-OWNER tx-sender))
    (map-set claimed { campaign-id: campaign-id, wallet: tx-sender } (get amount-per-claim camp))
    (map-set campaigns campaign-id (merge camp { claimed-total: (+ (get claimed-total camp) (get amount-per-claim camp)), claimants: (+ (get claimants camp) u1) }))
    (var-set total-distributed (+ (var-get total-distributed) (get amount-per-claim camp)))
    (ok (get amount-per-claim camp))))

(define-public (end-campaign (campaign-id uint))
  (let (
    (camp (unwrap! (map-get? campaigns campaign-id) ERR-NOT-FOUND))
    (remaining (- (get total-budget camp) (get claimed-total camp)))
  )
    (asserts! (is-eq tx-sender (get creator camp)) ERR-NOT-AUTHORIZED)
    (if (> remaining u0) (try! (stx-transfer? remaining CONTRACT-OWNER tx-sender)) true)
    (map-set campaigns campaign-id (merge camp { status: "ended" }))
    (ok remaining)))

(define-read-only (get-campaign (id uint)) (map-get? campaigns id))
(define-read-only (is-eligible (campaign-id uint) (wallet principal)) (ok (default-to false (map-get? eligible { campaign-id: campaign-id, wallet: wallet }))))
(define-read-only (has-claimed (campaign-id uint) (wallet principal)) (ok (is-some (map-get? claimed { campaign-id: campaign-id, wallet: wallet }))))
(define-read-only (get-campaign-count) (ok (var-get campaign-count)))
(define-read-only (get-total-distributed) (ok (var-get total-distributed)))
