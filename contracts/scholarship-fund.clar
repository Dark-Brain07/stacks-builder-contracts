;; Scholarship Fund Contract
;; Allocate and distribute educational scholarships
;; Halal - education and knowledge
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-APPLIED (err u405))
(define-constant ERR-FUND-EMPTY (err u406))

(define-data-var fund-balance uint u0)
(define-data-var scholarship-count uint u0)
(define-data-var total-awarded uint u0)
(define-data-var applicant-count uint u0)

(define-map scholarships uint { name: (string-utf8 100), amount: uint, field: (string-utf8 50), slots: uint, awarded: uint, active: bool })
(define-map applications { scholarship-id: uint, applicant: principal } { statement: (string-utf8 200), status: (string-ascii 20), applied-at: uint })

(define-public (create-scholarship (name (string-utf8 100)) (amount uint) (field (string-utf8 50)) (slots uint))
  (let ((id (+ (var-get scholarship-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set scholarships id { name: name, amount: amount, field: field, slots: slots, awarded: u0, active: true })
    (var-set scholarship-count id) (ok id)))

(define-public (donate-to-fund (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (var-set fund-balance (+ (var-get fund-balance) amount))
    (ok amount)))

(define-public (apply-scholarship (scholarship-id uint) (statement (string-utf8 200)))
  (let ((sch (unwrap! (map-get? scholarships scholarship-id) ERR-NOT-FOUND)))
    (asserts! (get active sch) ERR-NOT-FOUND)
    (asserts! (is-none (map-get? applications { scholarship-id: scholarship-id, applicant: tx-sender })) ERR-ALREADY-APPLIED)
    (map-set applications { scholarship-id: scholarship-id, applicant: tx-sender } { statement: statement, status: "pending", applied-at: stacks-block-height })
    (var-set applicant-count (+ (var-get applicant-count) u1))
    (ok true)))

(define-public (award-scholarship (scholarship-id uint) (student principal))
  (let (
    (sch (unwrap! (map-get? scholarships scholarship-id) ERR-NOT-FOUND))
    (app (unwrap! (map-get? applications { scholarship-id: scholarship-id, applicant: student }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (< (get awarded sch) (get slots sch)) ERR-FUND-EMPTY)
    (try! (stx-transfer? (get amount sch) tx-sender student))
    (map-set applications { scholarship-id: scholarship-id, applicant: student } (merge app { status: "awarded" }))
    (map-set scholarships scholarship-id (merge sch { awarded: (+ (get awarded sch) u1) }))
    (var-set total-awarded (+ (var-get total-awarded) (get amount sch)))
    (ok true)))

(define-read-only (get-scholarship (id uint)) (map-get? scholarships id))
(define-read-only (get-application (scholarship-id uint) (applicant principal)) (map-get? applications { scholarship-id: scholarship-id, applicant: applicant }))
(define-read-only (get-scholarship-count) (ok (var-get scholarship-count)))
(define-read-only (get-fund-balance) (ok (var-get fund-balance)))
(define-read-only (get-total-awarded) (ok (var-get total-awarded)))
