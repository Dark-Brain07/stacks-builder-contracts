;; Community Grants Contract
;; Allocate and manage community grants
;; Halal - community benefit
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-APPLIED (err u405))
(define-constant ERR-GRANT-CLOSED (err u406))

(define-data-var grant-count uint u0)
(define-data-var application-count uint u0)
(define-data-var total-funded uint u0)

(define-map grants uint { title: (string-utf8 100), budget: uint, allocated: uint, deadline: uint, active: bool })
(define-map applications uint {
  grant-id: uint, applicant: principal, proposal: (string-utf8 200),
  amount-requested: uint, status: (string-ascii 20), applied: uint
})
(define-map applicant-status { grant-id: uint, applicant: principal } uint)

(define-public (create-grant (title (string-utf8 100)) (budget uint) (duration uint))
  (let ((id (+ (var-get grant-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set grants id { title: title, budget: budget, allocated: u0, deadline: (+ stacks-block-height duration), active: true })
    (var-set grant-count id) (ok id)))

(define-public (apply-for-grant (grant-id uint) (proposal (string-utf8 200)) (amount uint))
  (let (
    (grant (unwrap! (map-get? grants grant-id) ERR-NOT-FOUND))
    (aid (+ (var-get application-count) u1))
  )
    (asserts! (get active grant) ERR-GRANT-CLOSED)
    (asserts! (< stacks-block-height (get deadline grant)) ERR-GRANT-CLOSED)
    (asserts! (is-none (map-get? applicant-status { grant-id: grant-id, applicant: tx-sender })) ERR-ALREADY-APPLIED)
    (map-set applications aid { grant-id: grant-id, applicant: tx-sender, proposal: proposal, amount-requested: amount, status: "pending", applied: stacks-block-height })
    (map-set applicant-status { grant-id: grant-id, applicant: tx-sender } aid)
    (var-set application-count aid) (ok aid)))

(define-public (approve-application (app-id uint))
  (let (
    (app (unwrap! (map-get? applications app-id) ERR-NOT-FOUND))
    (grant (unwrap! (map-get? grants (get grant-id app)) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? (get amount-requested app) tx-sender (get applicant app)))
    (map-set applications app-id (merge app { status: "approved" }))
    (map-set grants (get grant-id app) (merge grant { allocated: (+ (get allocated grant) (get amount-requested app)) }))
    (var-set total-funded (+ (var-get total-funded) (get amount-requested app)))
    (ok true)))

(define-public (reject-application (app-id uint))
  (let ((app (unwrap! (map-get? applications app-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set applications app-id (merge app { status: "rejected" })) (ok true)))

(define-public (close-grant (grant-id uint))
  (let ((grant (unwrap! (map-get? grants grant-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set grants grant-id (merge grant { active: false })) (ok true)))

(define-read-only (get-grant (id uint)) (map-get? grants id))
(define-read-only (get-application (id uint)) (map-get? applications id))
(define-read-only (get-grant-count) (ok (var-get grant-count)))
(define-read-only (get-application-count) (ok (var-get application-count)))
(define-read-only (get-total-funded) (ok (var-get total-funded)))
