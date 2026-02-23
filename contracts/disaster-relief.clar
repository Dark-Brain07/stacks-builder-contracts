;; Disaster Relief Contract
;; Disaster relief coordination and fund distribution
;; Halal - emergency mutual aid
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var disaster-count uint u0)
(define-data-var total-relief uint u0)

(define-map disasters uint {
  name: (string-utf8 100), location: (string-utf8 100), severity: uint,
  coordinator: principal, funds-raised: uint, funds-disbursed: uint,
  donors: uint, status: (string-ascii 20), declared: uint
})
(define-map disaster-donations { disaster-id: uint, donor: principal } uint)
(define-map relief-actions { disaster-id: uint, index: uint } { recipient: principal, amount: uint, purpose: (string-utf8 100), block: uint })
(define-map action-count uint uint)

(define-public (declare-disaster (name (string-utf8 100)) (location (string-utf8 100)) (severity uint))
  (let ((id (+ (var-get disaster-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set disasters id { name: name, location: location, severity: severity, coordinator: tx-sender, funds-raised: u0, funds-disbursed: u0, donors: u0, status: "active", declared: stacks-block-height })
    (var-set disaster-count id) (ok id)))

(define-public (donate-relief (disaster-id uint) (amount uint))
  (let (
    (disaster (unwrap! (map-get? disasters disaster-id) ERR-NOT-FOUND))
    (prev (default-to u0 (map-get? disaster-donations { disaster-id: disaster-id, donor: tx-sender })))
    (is-new (is-eq prev u0))
  )
    (try! (stx-transfer? amount tx-sender (get coordinator disaster)))
    (map-set disaster-donations { disaster-id: disaster-id, donor: tx-sender } (+ prev amount))
    (map-set disasters disaster-id (merge disaster { funds-raised: (+ (get funds-raised disaster) amount), donors: (if is-new (+ (get donors disaster) u1) (get donors disaster)) }))
    (var-set total-relief (+ (var-get total-relief) amount))
    (ok amount)))

(define-public (disburse-relief (disaster-id uint) (recipient principal) (amount uint) (purpose (string-utf8 100)))
  (let (
    (disaster (unwrap! (map-get? disasters disaster-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? action-count disaster-id)))
  )
    (asserts! (is-eq tx-sender (get coordinator disaster)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? amount tx-sender recipient))
    (map-set relief-actions { disaster-id: disaster-id, index: idx } { recipient: recipient, amount: amount, purpose: purpose, block: stacks-block-height })
    (map-set action-count disaster-id (+ idx u1))
    (map-set disasters disaster-id (merge disaster { funds-disbursed: (+ (get funds-disbursed disaster) amount) }))
    (ok amount)))

(define-public (close-relief (disaster-id uint))
  (let ((disaster (unwrap! (map-get? disasters disaster-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get coordinator disaster)) ERR-NOT-AUTHORIZED)
    (map-set disasters disaster-id (merge disaster { status: "closed" })) (ok true)))

(define-read-only (get-disaster (id uint)) (map-get? disasters id))
(define-read-only (get-donation (disaster-id uint) (donor principal)) (ok (default-to u0 (map-get? disaster-donations { disaster-id: disaster-id, donor: donor }))))
(define-read-only (get-action (disaster-id uint) (index uint)) (map-get? relief-actions { disaster-id: disaster-id, index: index }))
(define-read-only (get-disaster-count) (ok (var-get disaster-count)))
(define-read-only (get-total-relief) (ok (var-get total-relief)))
