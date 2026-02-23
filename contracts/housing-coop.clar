;; Housing Coop Contract
;; Housing cooperative management
;; Halal - cooperative housing
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-NOT-MEMBER (err u405))

(define-data-var member-count uint u0)
(define-data-var unit-count uint u0)
(define-data-var maintenance-pool uint u0)

(define-map members principal { name: (string-utf8 100), share: uint, unit-id: uint, dues-paid: uint, joined: uint })
(define-map units uint { unit-name: (string-utf8 50), occupant: (optional principal), monthly-dues: uint, maintenance-requests: uint })
(define-map maintenance-requests uint { unit-id: uint, requester: principal, description: (string-utf8 200), cost: uint, status: (string-ascii 20), filed: uint })
(define-data-var maint-count uint u0)

(define-public (add-unit (unit-name (string-utf8 50)) (monthly-dues uint))
  (let ((id (+ (var-get unit-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set units id { unit-name: unit-name, occupant: none, monthly-dues: monthly-dues, maintenance-requests: u0 })
    (var-set unit-count id) (ok id)))

(define-public (join-coop (name (string-utf8 100)) (share uint) (unit-id uint))
  (let ((unit (unwrap! (map-get? units unit-id) ERR-NOT-FOUND)))
    (asserts! (is-none (get occupant unit)) ERR-NOT-FOUND)
    (try! (stx-transfer? share tx-sender CONTRACT-OWNER))
    (map-set members tx-sender { name: name, share: share, unit-id: unit-id, dues-paid: u0, joined: stacks-block-height })
    (map-set units unit-id (merge unit { occupant: (some tx-sender) }))
    (var-set member-count (+ (var-get member-count) u1))
    (ok true)))

(define-public (pay-dues (amount uint))
  (let ((member (unwrap! (map-get? members tx-sender) ERR-NOT-MEMBER)))
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set members tx-sender (merge member { dues-paid: (+ (get dues-paid member) amount) }))
    (var-set maintenance-pool (+ (var-get maintenance-pool) amount))
    (ok amount)))

(define-public (file-maintenance (description (string-utf8 200)) (estimated-cost uint))
  (let (
    (member (unwrap! (map-get? members tx-sender) ERR-NOT-MEMBER))
    (mid (+ (var-get maint-count) u1))
  )
    (map-set maintenance-requests mid { unit-id: (get unit-id member), requester: tx-sender, description: description, cost: estimated-cost, status: "pending", filed: stacks-block-height })
    (var-set maint-count mid) (ok mid)))

(define-public (approve-maintenance (request-id uint))
  (let ((req (unwrap! (map-get? maintenance-requests request-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set maintenance-requests request-id (merge req { status: "approved" }))
    (var-set maintenance-pool (- (var-get maintenance-pool) (get cost req)))
    (ok (get cost req))))

(define-read-only (get-member (who principal)) (map-get? members who))
(define-read-only (get-unit (id uint)) (map-get? units id))
(define-read-only (get-maintenance (id uint)) (map-get? maintenance-requests id))
(define-read-only (get-member-count) (ok (var-get member-count)))
(define-read-only (get-unit-count) (ok (var-get unit-count)))
(define-read-only (get-maintenance-pool) (ok (var-get maintenance-pool)))
