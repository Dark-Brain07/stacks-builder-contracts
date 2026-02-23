;; Orphan Sponsor Contract
;; Orphan sponsorship and support program
;; Halal - caring for orphans (yateem)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-SPONSORED (err u405))

(define-data-var orphan-count uint u0)
(define-data-var total-sponsored uint u0)
(define-data-var total-support uint u0)

(define-map orphans uint {
  name: (string-utf8 100), age: uint, location: (string-utf8 100),
  monthly-need: uint, sponsor: (optional principal), total-received: uint,
  months-sponsored: uint, status: (string-ascii 20), registered: uint
})
(define-map sponsor-records principal { sponsoring: uint, total-given: uint, months-active: uint })
(define-map support-log { orphan-id: uint, index: uint } { sponsor: principal, amount: uint, block: uint })
(define-map support-count uint uint)

(define-public (register-orphan (name (string-utf8 100)) (age uint) (location (string-utf8 100)) (monthly-need uint))
  (let ((id (+ (var-get orphan-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set orphans id { name: name, age: age, location: location, monthly-need: monthly-need, sponsor: none, total-received: u0, months-sponsored: u0, status: "awaiting", registered: stacks-block-height })
    (var-set orphan-count id) (ok id)))

(define-public (sponsor-orphan (orphan-id uint))
  (let ((orphan (unwrap! (map-get? orphans orphan-id) ERR-NOT-FOUND)))
    (asserts! (is-none (get sponsor orphan)) ERR-ALREADY-SPONSORED)
    (map-set orphans orphan-id (merge orphan { sponsor: (some tx-sender), status: "sponsored" }))
    (map-set sponsor-records tx-sender (merge
      (default-to { sponsoring: u0, total-given: u0, months-active: u0 } (map-get? sponsor-records tx-sender))
      { sponsoring: (+ (default-to u0 (get sponsoring (map-get? sponsor-records tx-sender))) u1) }))
    (var-set total-sponsored (+ (var-get total-sponsored) u1))
    (ok true)))

(define-public (send-support (orphan-id uint) (amount uint))
  (let (
    (orphan (unwrap! (map-get? orphans orphan-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? support-count orphan-id)))
    (prev (default-to { sponsoring: u0, total-given: u0, months-active: u0 } (map-get? sponsor-records tx-sender)))
  )
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set support-log { orphan-id: orphan-id, index: idx } { sponsor: tx-sender, amount: amount, block: stacks-block-height })
    (map-set support-count orphan-id (+ idx u1))
    (map-set orphans orphan-id (merge orphan { total-received: (+ (get total-received orphan) amount), months-sponsored: (+ (get months-sponsored orphan) u1) }))
    (map-set sponsor-records tx-sender (merge prev { total-given: (+ (get total-given prev) amount), months-active: (+ (get months-active prev) u1) }))
    (var-set total-support (+ (var-get total-support) amount))
    (print { event: "orphan-support", orphan: orphan-id, sponsor: tx-sender, amount: amount })
    (ok amount)))

(define-read-only (get-orphan (id uint)) (map-get? orphans id))
(define-read-only (get-sponsor (who principal)) (map-get? sponsor-records who))
(define-read-only (get-support-entry (orphan-id uint) (index uint)) (map-get? support-log { orphan-id: orphan-id, index: index }))
(define-read-only (get-orphan-count) (ok (var-get orphan-count)))
(define-read-only (get-total-support) (ok (var-get total-support)))
