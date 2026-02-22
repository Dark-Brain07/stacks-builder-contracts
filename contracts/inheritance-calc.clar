;; Inheritance Calc Contract (Faraid)
;; Islamic inheritance distribution calculator
;; Halal - follows Quranic inheritance rules
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-DISTRIBUTED (err u405))

(define-data-var estate-count uint u0)
(define-data-var total-distributed uint u0)

(define-map estates uint {
  deceased-name: (string-utf8 100), executor: principal, total-value: uint,
  debts: uint, wasiyyah-pct: uint, distributed: uint,
  heirs: uint, status: (string-ascii 20), created: uint
})
(define-map heir-shares { estate-id: uint, heir: principal } {
  relationship: (string-ascii 20), share-pct: uint, amount: uint, paid: bool
})
(define-map heir-index { estate-id: uint, index: uint } principal)

(define-public (create-estate (name (string-utf8 100)) (total-value uint) (debts uint) (wasiyyah-pct uint))
  (let ((id (+ (var-get estate-count) u1)))
    (asserts! (<= wasiyyah-pct u33) ERR-NOT-AUTHORIZED) ;; Max 1/3 for wasiyyah
    (map-set estates id { deceased-name: name, executor: tx-sender, total-value: total-value, debts: debts, wasiyyah-pct: wasiyyah-pct, distributed: u0, heirs: u0, status: "open", created: stacks-block-height })
    (var-set estate-count id) (ok id)))

(define-public (add-heir (estate-id uint) (heir principal) (relationship (string-ascii 20)) (share-pct uint))
  (let (
    (estate (unwrap! (map-get? estates estate-id) ERR-NOT-FOUND))
    (idx (get heirs estate))
  )
    (asserts! (is-eq tx-sender (get executor estate)) ERR-NOT-AUTHORIZED)
    (let ((distributable (- (get total-value estate) (get debts estate)))
          (after-wasiyyah (- distributable (/ (* distributable (get wasiyyah-pct estate)) u100)))
          (heir-amount (/ (* after-wasiyyah share-pct) u100)))
      (map-set heir-shares { estate-id: estate-id, heir: heir } { relationship: relationship, share-pct: share-pct, amount: heir-amount, paid: false })
      (map-set heir-index { estate-id: estate-id, index: idx } heir)
      (map-set estates estate-id (merge estate { heirs: (+ idx u1) }))
      (ok heir-amount))))

(define-public (distribute-share (estate-id uint) (heir principal))
  (let (
    (estate (unwrap! (map-get? estates estate-id) ERR-NOT-FOUND))
    (share (unwrap! (map-get? heir-shares { estate-id: estate-id, heir: heir }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get executor estate)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get paid share)) ERR-ALREADY-DISTRIBUTED)
    (try! (stx-transfer? (get amount share) tx-sender heir))
    (map-set heir-shares { estate-id: estate-id, heir: heir } (merge share { paid: true }))
    (map-set estates estate-id (merge estate { distributed: (+ (get distributed estate) (get amount share)) }))
    (var-set total-distributed (+ (var-get total-distributed) (get amount share)))
    (ok (get amount share))))

(define-public (close-estate (estate-id uint))
  (let ((estate (unwrap! (map-get? estates estate-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get executor estate)) ERR-NOT-AUTHORIZED)
    (map-set estates estate-id (merge estate { status: "closed" })) (ok true)))

(define-read-only (get-estate (id uint)) (map-get? estates id))
(define-read-only (get-heir-share (estate-id uint) (heir principal)) (map-get? heir-shares { estate-id: estate-id, heir: heir }))
(define-read-only (get-heir-at (estate-id uint) (index uint)) (map-get? heir-index { estate-id: estate-id, index: index }))
(define-read-only (get-estate-count) (ok (var-get estate-count)))
(define-read-only (get-total-distributed) (ok (var-get total-distributed)))
