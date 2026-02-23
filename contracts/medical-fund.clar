;; Medical Fund Contract
;; Community medical expense crowdfunding
;; Halal - helping the sick
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var case-count uint u0)
(define-data-var total-raised uint u0)
(define-data-var total-disbursed uint u0)

(define-map medical-cases uint {
  patient-rep: principal, condition: (string-utf8 200), hospital: (string-utf8 100),
  goal: uint, raised: uint, disbursed: uint, donors: uint,
  verified: bool, status: (string-ascii 20), created: uint
})
(define-map case-donors { case-id: uint, donor: principal } uint)
(define-map medical-bills { case-id: uint, index: uint } { amount: uint, description: (string-utf8 100), provider: (string-utf8 100), block: uint })
(define-map bill-count uint uint)

(define-public (create-case (condition (string-utf8 200)) (hospital (string-utf8 100)) (goal uint))
  (let ((id (+ (var-get case-count) u1)))
    (map-set medical-cases id { patient-rep: tx-sender, condition: condition, hospital: hospital, goal: goal, raised: u0, disbursed: u0, donors: u0, verified: false, status: "active", created: stacks-block-height })
    (var-set case-count id) (ok id)))

(define-public (verify-case (case-id uint))
  (let ((c (unwrap! (map-get? medical-cases case-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set medical-cases case-id (merge c { verified: true })) (ok true)))

(define-public (donate-medical (case-id uint) (amount uint))
  (let (
    (c (unwrap! (map-get? medical-cases case-id) ERR-NOT-FOUND))
    (prev (default-to u0 (map-get? case-donors { case-id: case-id, donor: tx-sender })))
    (is-new (is-eq prev u0))
  )
    (asserts! (is-eq (get status c) "active") ERR-NOT-FOUND)
    (try! (stx-transfer? amount tx-sender (get patient-rep c)))
    (map-set case-donors { case-id: case-id, donor: tx-sender } (+ prev amount))
    (map-set medical-cases case-id (merge c { raised: (+ (get raised c) amount), donors: (if is-new (+ (get donors c) u1) (get donors c)) }))
    (var-set total-raised (+ (var-get total-raised) amount))
    (ok amount)))

(define-public (submit-bill (case-id uint) (amount uint) (description (string-utf8 100)) (provider (string-utf8 100)))
  (let (
    (c (unwrap! (map-get? medical-cases case-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? bill-count case-id)))
  )
    (asserts! (is-eq tx-sender (get patient-rep c)) ERR-NOT-AUTHORIZED)
    (map-set medical-bills { case-id: case-id, index: idx } { amount: amount, description: description, provider: provider, block: stacks-block-height })
    (map-set bill-count case-id (+ idx u1))
    (map-set medical-cases case-id (merge c { disbursed: (+ (get disbursed c) amount) }))
    (var-set total-disbursed (+ (var-get total-disbursed) amount))
    (ok idx)))

(define-public (close-case (case-id uint))
  (let ((c (unwrap! (map-get? medical-cases case-id) ERR-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender (get patient-rep c)) (is-eq tx-sender CONTRACT-OWNER)) ERR-NOT-AUTHORIZED)
    (map-set medical-cases case-id (merge c { status: "closed" })) (ok true)))

(define-read-only (get-case (id uint)) (map-get? medical-cases id))
(define-read-only (get-donor-amount (case-id uint) (donor principal)) (ok (default-to u0 (map-get? case-donors { case-id: case-id, donor: donor }))))
(define-read-only (get-bill (case-id uint) (index uint)) (map-get? medical-bills { case-id: case-id, index: index }))
(define-read-only (get-case-count) (ok (var-get case-count)))
(define-read-only (get-total-raised) (ok (var-get total-raised)))
