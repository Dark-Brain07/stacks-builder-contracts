;; Education Fund Contract
;; Community education fund for students
;; Halal - investing in knowledge (ilm)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var fund-balance uint u0)
(define-data-var student-count uint u0)
(define-data-var total-disbursed uint u0)

(define-map students uint {
  name: (string-utf8 100), institution: (string-utf8 100), program: (string-utf8 100),
  needed: uint, received: uint, gpa: uint, status: (string-ascii 20), enrolled: uint
})
(define-map contributors principal { total: uint, donations: uint })
(define-map disbursements { student-id: uint, index: uint } { amount: uint, purpose: (string-utf8 100), block: uint })
(define-map disbursement-count uint uint)

(define-public (contribute (amount uint))
  (let ((prev (default-to { total: u0, donations: u0 } (map-get? contributors tx-sender))))
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set contributors tx-sender { total: (+ (get total prev) amount), donations: (+ (get donations prev) u1) })
    (var-set fund-balance (+ (var-get fund-balance) amount))
    (ok amount)))

(define-public (enroll-student (name (string-utf8 100)) (institution (string-utf8 100)) (program (string-utf8 100)) (needed uint) (gpa uint))
  (let ((id (+ (var-get student-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set students id { name: name, institution: institution, program: program, needed: needed, received: u0, gpa: gpa, status: "active", enrolled: stacks-block-height })
    (var-set student-count id) (ok id)))

(define-public (disburse (student-id uint) (amount uint) (purpose (string-utf8 100)))
  (let (
    (student (unwrap! (map-get? students student-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? disbursement-count student-id)))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set disbursements { student-id: student-id, index: idx } { amount: amount, purpose: purpose, block: stacks-block-height })
    (map-set disbursement-count student-id (+ idx u1))
    (map-set students student-id (merge student { received: (+ (get received student) amount) }))
    (var-set fund-balance (- (var-get fund-balance) amount))
    (var-set total-disbursed (+ (var-get total-disbursed) amount))
    (ok amount)))

(define-public (update-gpa (student-id uint) (new-gpa uint))
  (let ((student (unwrap! (map-get? students student-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set students student-id (merge student { gpa: new-gpa })) (ok true)))

(define-public (graduate-student (student-id uint))
  (let ((student (unwrap! (map-get? students student-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set students student-id (merge student { status: "graduated" })) (ok true)))

(define-read-only (get-student (id uint)) (map-get? students id))
(define-read-only (get-contributor (who principal)) (map-get? contributors who))
(define-read-only (get-disbursement (student-id uint) (index uint)) (map-get? disbursements { student-id: student-id, index: index }))
(define-read-only (get-fund-balance) (ok (var-get fund-balance)))
(define-read-only (get-total-disbursed) (ok (var-get total-disbursed)))
(define-read-only (get-student-count) (ok (var-get student-count)))
