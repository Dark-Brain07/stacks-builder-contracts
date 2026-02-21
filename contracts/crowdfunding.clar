;; Crowdfunding Contract
;; Create and fund campaigns with goal-based releases
;; Built by rajuice for Stacks Builder Rewards

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-CAMPAIGN-NOT-FOUND (err u404))
(define-constant ERR-CAMPAIGN-ENDED (err u405))
(define-constant ERR-GOAL-NOT-MET (err u406))
(define-constant ERR-ALREADY-CLAIMED (err u407))
(define-constant ERR-CAMPAIGN-ACTIVE (err u408))

(define-data-var campaign-count uint u0)

(define-map campaigns uint {
  creator: principal,
  title: (string-utf8 100),
  goal: uint,
  raised: uint,
  deadline: uint,
  claimed: bool
})

(define-map contributions { campaign-id: uint, contributor: principal } uint)

(define-public (create-campaign (title (string-utf8 100)) (goal uint) (duration uint))
  (let ((campaign-id (+ (var-get campaign-count) u1)))
    (map-set campaigns campaign-id {
      creator: tx-sender,
      title: title,
      goal: goal,
      raised: u0,
      deadline: (+ stacks-block-height duration),
      claimed: false
    })
    (var-set campaign-count campaign-id)
    (ok campaign-id)))

(define-public (contribute (campaign-id uint) (amount uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) ERR-CAMPAIGN-NOT-FOUND))
        (current (default-to u0 (map-get? contributions { campaign-id: campaign-id, contributor: tx-sender }))))
    (asserts! (< stacks-block-height (get deadline campaign)) ERR-CAMPAIGN-ENDED)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set contributions { campaign-id: campaign-id, contributor: tx-sender } (+ current amount))
    (map-set campaigns campaign-id (merge campaign { raised: (+ (get raised campaign) amount) }))
    (ok (+ (get raised campaign) amount))))

(define-public (claim-funds (campaign-id uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) ERR-CAMPAIGN-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get creator campaign)) ERR-NOT-AUTHORIZED)
    (asserts! (>= stacks-block-height (get deadline campaign)) ERR-CAMPAIGN-ACTIVE)
    (asserts! (>= (get raised campaign) (get goal campaign)) ERR-GOAL-NOT-MET)
    (asserts! (not (get claimed campaign)) ERR-ALREADY-CLAIMED)
    (try! (as-contract (stx-transfer? (get raised campaign) tx-sender (get creator campaign))))
    (map-set campaigns campaign-id (merge campaign { claimed: true }))
    (ok (get raised campaign))))

(define-public (refund (campaign-id uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) ERR-CAMPAIGN-NOT-FOUND))
        (contributed (default-to u0 (map-get? contributions { campaign-id: campaign-id, contributor: tx-sender }))))
    (asserts! (>= stacks-block-height (get deadline campaign)) ERR-CAMPAIGN-ACTIVE)
    (asserts! (< (get raised campaign) (get goal campaign)) ERR-GOAL-NOT-MET)
    (asserts! (> contributed u0) ERR-NOT-AUTHORIZED)
    (try! (as-contract (stx-transfer? contributed tx-sender tx-sender)))
    (map-set contributions { campaign-id: campaign-id, contributor: tx-sender } u0)
    (ok contributed)))

(define-read-only (get-campaign (campaign-id uint))
  (map-get? campaigns campaign-id))

(define-read-only (get-contribution (campaign-id uint) (contributor principal))
  (ok (default-to u0 (map-get? contributions { campaign-id: campaign-id, contributor: contributor }))))

(define-read-only (get-campaign-count)
  (ok (var-get campaign-count)))
