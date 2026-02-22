;; Warranty Tracker Contract
;; Track product warranties on-chain
;; Halal - consumer protection
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-EXPIRED (err u405))
(define-constant ERR-ALREADY-CLAIMED (err u406))

(define-data-var warranty-count uint u0)
(define-data-var claim-count uint u0)

(define-map warranties uint { issuer: principal, owner: principal, product: (string-utf8 100), expires: uint, active: bool })
(define-map warranty-claims uint { warranty-id: uint, description: (string-utf8 200), status: (string-ascii 20), filed: uint })
(define-map warranty-claim-count uint uint)

(define-public (issue-warranty (owner principal) (product (string-utf8 100)) (duration uint))
  (let ((id (+ (var-get warranty-count) u1)))
    (map-set warranties id { issuer: tx-sender, owner: owner, product: product, expires: (+ stacks-block-height duration), active: true })
    (var-set warranty-count id) (ok id)))

(define-public (transfer-warranty (id uint) (new-owner principal))
  (let ((w (unwrap! (map-get? warranties id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner w)) ERR-NOT-AUTHORIZED)
    (map-set warranties id (merge w { owner: new-owner })) (ok true)))

(define-public (file-claim (warranty-id uint) (description (string-utf8 200)))
  (let (
    (w (unwrap! (map-get? warranties warranty-id) ERR-NOT-FOUND))
    (cid (+ (var-get claim-count) u1))
  )
    (asserts! (is-eq tx-sender (get owner w)) ERR-NOT-AUTHORIZED)
    (asserts! (get active w) ERR-EXPIRED)
    (asserts! (< stacks-block-height (get expires w)) ERR-EXPIRED)
    (map-set warranty-claims cid { warranty-id: warranty-id, description: description, status: "pending", filed: stacks-block-height })
    (map-set warranty-claim-count warranty-id (+ (default-to u0 (map-get? warranty-claim-count warranty-id)) u1))
    (var-set claim-count cid) (ok cid)))

(define-public (resolve-claim (claim-id uint) (resolution (string-ascii 20)))
  (let ((claim (unwrap! (map-get? warranty-claims claim-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set warranty-claims claim-id (merge claim { status: resolution })) (ok true)))

(define-read-only (get-warranty (id uint)) (map-get? warranties id))
(define-read-only (get-claim (id uint)) (map-get? warranty-claims id))
(define-read-only (get-warranty-count) (ok (var-get warranty-count)))
(define-read-only (get-claims-for-warranty (warranty-id uint)) (ok (default-to u0 (map-get? warranty-claim-count warranty-id))))
(define-read-only (is-valid (id uint))
  (match (map-get? warranties id) w (ok (and (get active w) (< stacks-block-height (get expires w)))) (ok false)))
