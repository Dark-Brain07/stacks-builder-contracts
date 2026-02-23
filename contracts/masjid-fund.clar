;; Masjid Fund Contract
;; Mosque construction and maintenance fund
;; Halal - building houses of worship
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var project-count uint u0)
(define-data-var total-raised uint u0)

(define-map masjid-projects uint {
  name: (string-utf8 100), location: (string-utf8 100), category: (string-ascii 20),
  goal: uint, raised: uint, donors: uint, spent: uint,
  trustee: principal, status: (string-ascii 20), created: uint
})
(define-map donations { project-id: uint, donor: principal } { total: uint, count: uint })
(define-map expenses { project-id: uint, index: uint } { amount: uint, description: (string-utf8 100), vendor: (string-utf8 100), block: uint })
(define-map expense-count uint uint)

(define-public (create-project (name (string-utf8 100)) (location (string-utf8 100)) (category (string-ascii 20)) (goal uint))
  (let ((id (+ (var-get project-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set masjid-projects id { name: name, location: location, category: category, goal: goal, raised: u0, donors: u0, spent: u0, trustee: tx-sender, status: "active", created: stacks-block-height })
    (var-set project-count id) (ok id)))

(define-public (donate (project-id uint) (amount uint))
  (let (
    (project (unwrap! (map-get? masjid-projects project-id) ERR-NOT-FOUND))
    (prev (default-to { total: u0, count: u0 } (map-get? donations { project-id: project-id, donor: tx-sender })))
    (is-new (is-eq (get total prev) u0))
  )
    (try! (stx-transfer? amount tx-sender (get trustee project)))
    (map-set donations { project-id: project-id, donor: tx-sender } { total: (+ (get total prev) amount), count: (+ (get count prev) u1) })
    (map-set masjid-projects project-id (merge project { raised: (+ (get raised project) amount), donors: (if is-new (+ (get donors project) u1) (get donors project)) }))
    (var-set total-raised (+ (var-get total-raised) amount))
    (print { event: "masjid-donation", project: project-id, donor: tx-sender, amount: amount })
    (ok amount)))

(define-public (record-expense (project-id uint) (amount uint) (description (string-utf8 100)) (vendor (string-utf8 100)))
  (let (
    (project (unwrap! (map-get? masjid-projects project-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? expense-count project-id)))
  )
    (asserts! (is-eq tx-sender (get trustee project)) ERR-NOT-AUTHORIZED)
    (map-set expenses { project-id: project-id, index: idx } { amount: amount, description: description, vendor: vendor, block: stacks-block-height })
    (map-set expense-count project-id (+ idx u1))
    (map-set masjid-projects project-id (merge project { spent: (+ (get spent project) amount) }))
    (ok idx)))

(define-public (complete-project (project-id uint))
  (let ((project (unwrap! (map-get? masjid-projects project-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get trustee project)) ERR-NOT-AUTHORIZED)
    (map-set masjid-projects project-id (merge project { status: "completed" })) (ok true)))

(define-read-only (get-project (id uint)) (map-get? masjid-projects id))
(define-read-only (get-donation (project-id uint) (donor principal)) (map-get? donations { project-id: project-id, donor: donor }))
(define-read-only (get-expense (project-id uint) (index uint)) (map-get? expenses { project-id: project-id, index: index }))
(define-read-only (get-project-count) (ok (var-get project-count)))
(define-read-only (get-total-raised) (ok (var-get total-raised)))
