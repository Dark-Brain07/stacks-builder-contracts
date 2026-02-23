;; Content License Contract
;; License digital content with on-chain rights management
;; Halal - intellectual property trade
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-LICENSED (err u405))

(define-data-var content-count uint u0)
(define-data-var license-count uint u0)

(define-map contents uint { creator: principal, title: (string-utf8 100), license-fee: uint, total-licenses: uint, created: uint })
(define-map licenses uint { content-id: uint, licensee: principal, granted-at: uint, expires-at: uint })
(define-map user-licenses { user: principal, content: uint } uint)

(define-public (register-content (title (string-utf8 100)) (fee uint))
  (let ((id (+ (var-get content-count) u1)))
    (map-set contents id { creator: tx-sender, title: title, license-fee: fee, total-licenses: u0, created: stacks-block-height })
    (var-set content-count id) (ok id)))

(define-public (purchase-license (content-id uint) (duration uint))
  (let (
    (content (unwrap! (map-get? contents content-id) ERR-NOT-FOUND))
    (lid (+ (var-get license-count) u1))
  )
    (asserts! (is-none (map-get? user-licenses { user: tx-sender, content: content-id })) ERR-ALREADY-LICENSED)
    (try! (stx-transfer? (get license-fee content) tx-sender (get creator content)))
    (map-set licenses lid { content-id: content-id, licensee: tx-sender, granted-at: stacks-block-height, expires-at: (+ stacks-block-height duration) })
    (map-set user-licenses { user: tx-sender, content: content-id } lid)
    (map-set contents content-id (merge content { total-licenses: (+ (get total-licenses content) u1) }))
    (var-set license-count lid) (ok lid)))

(define-public (update-fee (content-id uint) (new-fee uint))
  (let ((content (unwrap! (map-get? contents content-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get creator content)) ERR-NOT-AUTHORIZED)
    (map-set contents content-id (merge content { license-fee: new-fee })) (ok true)))

(define-read-only (get-content (id uint)) (map-get? contents id))
(define-read-only (get-license (id uint)) (map-get? licenses id))
(define-read-only (get-content-count) (ok (var-get content-count)))
(define-read-only (get-license-count) (ok (var-get license-count)))
(define-read-only (has-license (user principal) (content uint))
  (match (map-get? user-licenses { user: user, content: content })
    lid (match (map-get? licenses lid) l (ok (>= (get expires-at l) stacks-block-height)) (ok false))
    (ok false)))
