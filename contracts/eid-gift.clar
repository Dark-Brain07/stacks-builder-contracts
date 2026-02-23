;; Eid Gift Contract
;; Eid gift exchange and distribution
;; Halal - celebrating Eid with generosity
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-CLAIMED (err u405))

(define-data-var campaign-count uint u0)
(define-data-var total-gifts uint u0)

(define-map campaigns uint { organizer: principal, eid-type: (string-ascii 20), budget: uint, spent: uint, recipients: uint, max-recipients: uint, gift-amount: uint, status: (string-ascii 20) })
(define-map gift-recipients { campaign-id: uint, recipient: principal } { amount: uint, claimed: uint })
(define-map campaign-donors { campaign-id: uint, donor: principal } uint)

(define-public (create-campaign (eid-type (string-ascii 20)) (budget uint) (max-recipients uint) (gift-amount uint))
  (let ((id (+ (var-get campaign-count) u1)))
    (try! (stx-transfer? budget tx-sender CONTRACT-OWNER))
    (map-set campaigns id { organizer: tx-sender, eid-type: eid-type, budget: budget, spent: u0, recipients: u0, max-recipients: max-recipients, gift-amount: gift-amount, status: "active" })
    (var-set campaign-count id) (ok id)))

(define-public (donate-to-campaign (campaign-id uint) (amount uint))
  (let (
    (camp (unwrap! (map-get? campaigns campaign-id) ERR-NOT-FOUND))
    (prev (default-to u0 (map-get? campaign-donors { campaign-id: campaign-id, donor: tx-sender })))
  )
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set campaign-donors { campaign-id: campaign-id, donor: tx-sender } (+ prev amount))
    (map-set campaigns campaign-id (merge camp { budget: (+ (get budget camp) amount) })) (ok amount)))

(define-public (distribute-gift (campaign-id uint) (recipient principal))
  (let ((camp (unwrap! (map-get? campaigns campaign-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer camp)) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? gift-recipients { campaign-id: campaign-id, recipient: recipient })) ERR-ALREADY-CLAIMED)
    (asserts! (< (get recipients camp) (get max-recipients camp)) ERR-NOT-FOUND)
    (try! (stx-transfer? (get gift-amount camp) CONTRACT-OWNER recipient))
    (map-set gift-recipients { campaign-id: campaign-id, recipient: recipient } { amount: (get gift-amount camp), claimed: stacks-block-height })
    (map-set campaigns campaign-id (merge camp { spent: (+ (get spent camp) (get gift-amount camp)), recipients: (+ (get recipients camp) u1) }))
    (var-set total-gifts (+ (var-get total-gifts) u1)) (ok true)))

(define-public (close-campaign (campaign-id uint))
  (let (
    (camp (unwrap! (map-get? campaigns campaign-id) ERR-NOT-FOUND))
    (remaining (- (get budget camp) (get spent camp)))
  )
    (asserts! (is-eq tx-sender (get organizer camp)) ERR-NOT-AUTHORIZED)
    (if (> remaining u0) (try! (stx-transfer? remaining CONTRACT-OWNER tx-sender)) true)
    (map-set campaigns campaign-id (merge camp { status: "closed" })) (ok remaining)))

(define-read-only (get-campaign (id uint)) (map-get? campaigns id))
(define-read-only (get-recipient (campaign-id uint) (who principal)) (map-get? gift-recipients { campaign-id: campaign-id, recipient: who }))
(define-read-only (get-campaign-count) (ok (var-get campaign-count)))
(define-read-only (get-total-gifts) (ok (var-get total-gifts)))
