;; Winter Relief Contract
;; Winter relief supplies distribution
;; Halal - protecting from cold
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var campaign-count uint u0)
(define-data-var total-raised uint u0)
(define-data-var families-helped uint u0)

(define-map campaigns uint { organizer: principal, region: (string-utf8 100), goal: uint, raised: uint, packages-sent: uint, status: (string-ascii 20), created: uint })
(define-map campaign-donations { campaign-id: uint, donor: principal } uint)
(define-map relief-packages { campaign-id: uint, index: uint } { family-size: uint, items: (string-utf8 200), cost: uint, block: uint })
(define-map package-count uint uint)

(define-public (create-campaign (region (string-utf8 100)) (goal uint))
  (let ((id (+ (var-get campaign-count) u1)))
    (map-set campaigns id { organizer: tx-sender, region: region, goal: goal, raised: u0, packages-sent: u0, status: "active", created: stacks-block-height })
    (var-set campaign-count id) (ok id)))

(define-public (donate-relief (campaign-id uint) (amount uint))
  (let (
    (c (unwrap! (map-get? campaigns campaign-id) ERR-NOT-FOUND))
    (prev (default-to u0 (map-get? campaign-donations { campaign-id: campaign-id, donor: tx-sender })))
  )
    (try! (stx-transfer? amount tx-sender (get organizer c)))
    (map-set campaign-donations { campaign-id: campaign-id, donor: tx-sender } (+ prev amount))
    (map-set campaigns campaign-id (merge c { raised: (+ (get raised c) amount) }))
    (var-set total-raised (+ (var-get total-raised) amount)) (ok amount)))

(define-public (send-package (campaign-id uint) (family-size uint) (items (string-utf8 200)) (cost uint))
  (let (
    (c (unwrap! (map-get? campaigns campaign-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? package-count campaign-id)))
  )
    (asserts! (is-eq tx-sender (get organizer c)) ERR-NOT-AUTHORIZED)
    (map-set relief-packages { campaign-id: campaign-id, index: idx } { family-size: family-size, items: items, cost: cost, block: stacks-block-height })
    (map-set package-count campaign-id (+ idx u1))
    (map-set campaigns campaign-id (merge c { packages-sent: (+ (get packages-sent c) u1) }))
    (var-set families-helped (+ (var-get families-helped) u1)) (ok idx)))

(define-public (close-campaign (campaign-id uint))
  (let ((c (unwrap! (map-get? campaigns campaign-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer c)) ERR-NOT-AUTHORIZED)
    (map-set campaigns campaign-id (merge c { status: "closed" })) (ok true)))

(define-read-only (get-campaign (id uint)) (map-get? campaigns id))
(define-read-only (get-donor (campaign-id uint) (who principal)) (ok (default-to u0 (map-get? campaign-donations { campaign-id: campaign-id, donor: who }))))
(define-read-only (get-package (campaign-id uint) (index uint)) (map-get? relief-packages { campaign-id: campaign-id, index: index }))
(define-read-only (get-campaign-count) (ok (var-get campaign-count)))
(define-read-only (get-families-helped) (ok (var-get families-helped)))
