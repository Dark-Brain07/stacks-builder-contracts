;; Crowdsource Data Contract
;; Collect and reward crowdsourced data contributions
;; Halal - knowledge sharing
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-SUBMITTED (err u405))

(define-data-var campaign-count uint u0)
(define-data-var submission-count uint u0)
(define-data-var total-rewards-paid uint u0)

(define-map campaigns uint {
  creator: principal, title: (string-utf8 100), reward-per-entry: uint,
  max-entries: uint, entries: uint, budget: uint, spent: uint, active: bool
})
(define-map submissions uint { campaign-id: uint, contributor: principal, data-hash: (buff 32), validated: bool, submitted: uint })
(define-map has-submitted { campaign-id: uint, contributor: principal } bool)

(define-public (create-campaign (title (string-utf8 100)) (reward uint) (max-entries uint) (budget uint))
  (let ((id (+ (var-get campaign-count) u1)))
    (try! (stx-transfer? budget tx-sender CONTRACT-OWNER))
    (map-set campaigns id { creator: tx-sender, title: title, reward-per-entry: reward, max-entries: max-entries, entries: u0, budget: budget, spent: u0, active: true })
    (var-set campaign-count id) (ok id)))

(define-public (submit-data (campaign-id uint) (data-hash (buff 32)))
  (let (
    (campaign (unwrap! (map-get? campaigns campaign-id) ERR-NOT-FOUND))
    (sid (+ (var-get submission-count) u1))
  )
    (asserts! (get active campaign) ERR-NOT-FOUND)
    (asserts! (< (get entries campaign) (get max-entries campaign)) ERR-NOT-FOUND)
    (asserts! (is-none (map-get? has-submitted { campaign-id: campaign-id, contributor: tx-sender })) ERR-ALREADY-SUBMITTED)
    (map-set submissions sid { campaign-id: campaign-id, contributor: tx-sender, data-hash: data-hash, validated: false, submitted: stacks-block-height })
    (map-set has-submitted { campaign-id: campaign-id, contributor: tx-sender } true)
    (map-set campaigns campaign-id (merge campaign { entries: (+ (get entries campaign) u1) }))
    (var-set submission-count sid) (ok sid)))

(define-public (validate-and-reward (submission-id uint))
  (let (
    (sub (unwrap! (map-get? submissions submission-id) ERR-NOT-FOUND))
    (campaign (unwrap! (map-get? campaigns (get campaign-id sub)) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? (get reward-per-entry campaign) tx-sender (get contributor sub)))
    (map-set submissions submission-id (merge sub { validated: true }))
    (map-set campaigns (get campaign-id sub) (merge campaign { spent: (+ (get spent campaign) (get reward-per-entry campaign)) }))
    (var-set total-rewards-paid (+ (var-get total-rewards-paid) (get reward-per-entry campaign)))
    (ok true)))

(define-public (close-campaign (campaign-id uint))
  (let ((c (unwrap! (map-get? campaigns campaign-id) ERR-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender (get creator c)) (is-eq tx-sender CONTRACT-OWNER)) ERR-NOT-AUTHORIZED)
    (map-set campaigns campaign-id (merge c { active: false })) (ok true)))

(define-read-only (get-campaign (id uint)) (map-get? campaigns id))
(define-read-only (get-submission (id uint)) (map-get? submissions id))
(define-read-only (get-campaign-count) (ok (var-get campaign-count)))
(define-read-only (get-total-rewards) (ok (var-get total-rewards-paid)))
