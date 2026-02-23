;; Trust Fund Contract
;; Managed trust fund with beneficiaries
;; Halal - amanah (trust)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-LOCKED (err u405))

(define-data-var trust-count uint u0)
(define-data-var total-in-trust uint u0)

(define-map trusts uint {
  grantor: principal, trustee: principal, beneficiary: principal,
  balance: uint, released: uint, unlock-block: uint,
  purpose: (string-utf8 200), status: (string-ascii 20), created: uint
})
(define-map release-log { trust-id: uint, index: uint } { amount: uint, block: uint })
(define-map release-count uint uint)

(define-public (create-trust (trustee principal) (beneficiary principal) (amount uint) (lock-blocks uint) (purpose (string-utf8 200)))
  (let ((id (+ (var-get trust-count) u1)))
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set trusts id { grantor: tx-sender, trustee: trustee, beneficiary: beneficiary, balance: amount, released: u0, unlock-block: (+ stacks-block-height lock-blocks), purpose: purpose, status: "active", created: stacks-block-height })
    (var-set trust-count id)
    (var-set total-in-trust (+ (var-get total-in-trust) amount))
    (ok id)))

(define-public (add-funds (trust-id uint) (amount uint))
  (let ((trust (unwrap! (map-get? trusts trust-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get grantor trust)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set trusts trust-id (merge trust { balance: (+ (get balance trust) amount) }))
    (var-set total-in-trust (+ (var-get total-in-trust) amount))
    (ok amount)))

(define-public (release-funds (trust-id uint) (amount uint))
  (let (
    (trust (unwrap! (map-get? trusts trust-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? release-count trust-id)))
  )
    (asserts! (is-eq tx-sender (get trustee trust)) ERR-NOT-AUTHORIZED)
    (asserts! (>= stacks-block-height (get unlock-block trust)) ERR-LOCKED)
    (asserts! (<= amount (get balance trust)) ERR-NOT-FOUND)
    (try! (stx-transfer? amount CONTRACT-OWNER (get beneficiary trust)))
    (map-set release-log { trust-id: trust-id, index: idx } { amount: amount, block: stacks-block-height })
    (map-set release-count trust-id (+ idx u1))
    (map-set trusts trust-id (merge trust { balance: (- (get balance trust) amount), released: (+ (get released trust) amount) }))
    (ok amount)))

(define-public (close-trust (trust-id uint))
  (let ((trust (unwrap! (map-get? trusts trust-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get grantor trust)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get balance trust) u0) ERR-NOT-FOUND)
    (map-set trusts trust-id (merge trust { status: "closed" })) (ok true)))

(define-read-only (get-trust (id uint)) (map-get? trusts id))
(define-read-only (get-release (trust-id uint) (index uint)) (map-get? release-log { trust-id: trust-id, index: index }))
(define-read-only (get-trust-count) (ok (var-get trust-count)))
(define-read-only (get-total-in-trust) (ok (var-get total-in-trust)))
(define-read-only (is-unlocked (id uint))
  (match (map-get? trusts id) t (ok (>= stacks-block-height (get unlock-block t))) (ok false)))
