;; Fund Raiser Contract
;; Event-based fundraising campaigns
;; Halal - community fundraising
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ENDED (err u405))
(define-constant ERR-GOAL-NOT-MET (err u406))

(define-data-var campaign-count uint u0)
(define-data-var total-raised-all uint u0)

(define-map campaigns uint {
  organizer: principal, title: (string-utf8 100), cause: (string-utf8 200),
  goal: uint, raised: uint, donors: uint, deadline: uint,
  withdrawn: bool, status: (string-ascii 20)
})
(define-map donations { campaign-id: uint, donor: principal } uint)

(define-public (create-campaign (title (string-utf8 100)) (cause (string-utf8 200)) (goal uint) (duration uint))
  (let ((id (+ (var-get campaign-count) u1)))
    (map-set campaigns id {
      organizer: tx-sender, title: title, cause: cause,
      goal: goal, raised: u0, donors: u0,
      deadline: (+ stacks-block-height duration), withdrawn: false, status: "active"
    })
    (var-set campaign-count id) (ok id)))

(define-public (donate (campaign-id uint) (amount uint))
  (let (
    (campaign (unwrap! (map-get? campaigns campaign-id) ERR-NOT-FOUND))
    (prev (default-to u0 (map-get? donations { campaign-id: campaign-id, donor: tx-sender })))
    (is-new (is-eq prev u0))
  )
    (asserts! (is-eq (get status campaign) "active") ERR-ENDED)
    (asserts! (< stacks-block-height (get deadline campaign)) ERR-ENDED)
    (try! (stx-transfer? amount tx-sender (get organizer campaign)))
    (map-set donations { campaign-id: campaign-id, donor: tx-sender } (+ prev amount))
    (map-set campaigns campaign-id (merge campaign {
      raised: (+ (get raised campaign) amount),
      donors: (if is-new (+ (get donors campaign) u1) (get donors campaign))
    }))
    (var-set total-raised-all (+ (var-get total-raised-all) amount))
    (ok amount)))

(define-public (end-campaign (campaign-id uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer campaign)) ERR-NOT-AUTHORIZED)
    (map-set campaigns campaign-id (merge campaign {
      status: (if (>= (get raised campaign) (get goal campaign)) "goal-met" "ended")
    }))
    (ok true)))

(define-public (extend-deadline (campaign-id uint) (extra-blocks uint))
  (let ((campaign (unwrap! (map-get? campaigns campaign-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer campaign)) ERR-NOT-AUTHORIZED)
    (map-set campaigns campaign-id (merge campaign { deadline: (+ (get deadline campaign) extra-blocks) }))
    (ok true)))

(define-read-only (get-campaign (id uint)) (map-get? campaigns id))
(define-read-only (get-donation (campaign-id uint) (donor principal)) (ok (default-to u0 (map-get? donations { campaign-id: campaign-id, donor: donor }))))
(define-read-only (get-campaign-count) (ok (var-get campaign-count)))
(define-read-only (get-total-raised) (ok (var-get total-raised-all)))
