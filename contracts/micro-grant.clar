;; Micro Grant Contract
;; Small grants for individuals and small projects
;; Halal - community support
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-APPLIED (err u405))

(define-data-var grant-pool uint u0)
(define-data-var application-count uint u0)
(define-data-var grants-awarded uint u0)

(define-map applications uint {
  applicant: principal, title: (string-utf8 100), description: (string-utf8 200),
  amount-requested: uint, amount-awarded: uint,
  status: (string-ascii 20), applied: uint
})
(define-map reviewers principal bool)
(define-map applicant-history principal uint)

(map-set reviewers CONTRACT-OWNER true)

(define-public (fund-pool (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (var-set grant-pool (+ (var-get grant-pool) amount)) (ok amount)))

(define-public (add-reviewer (r principal))
  (begin (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED) (map-set reviewers r true) (ok true)))

(define-public (apply-grant (title (string-utf8 100)) (description (string-utf8 200)) (amount uint))
  (let ((id (+ (var-get application-count) u1)))
    (map-set applications id { applicant: tx-sender, title: title, description: description, amount-requested: amount, amount-awarded: u0, status: "pending", applied: stacks-block-height })
    (map-set applicant-history tx-sender (+ (default-to u0 (map-get? applicant-history tx-sender)) u1))
    (var-set application-count id) (ok id)))

(define-public (approve-grant (app-id uint) (award-amount uint))
  (let ((app (unwrap! (map-get? applications app-id) ERR-NOT-FOUND)))
    (asserts! (default-to false (map-get? reviewers tx-sender)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? award-amount CONTRACT-OWNER (get applicant app)))
    (map-set applications app-id (merge app { amount-awarded: award-amount, status: "approved" }))
    (var-set grant-pool (- (var-get grant-pool) award-amount))
    (var-set grants-awarded (+ (var-get grants-awarded) u1))
    (ok award-amount)))

(define-public (reject-grant (app-id uint))
  (let ((app (unwrap! (map-get? applications app-id) ERR-NOT-FOUND)))
    (asserts! (default-to false (map-get? reviewers tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set applications app-id (merge app { status: "rejected" })) (ok true)))

(define-read-only (get-application (id uint)) (map-get? applications id))
(define-read-only (get-grant-pool) (ok (var-get grant-pool)))
(define-read-only (get-application-count) (ok (var-get application-count)))
(define-read-only (get-grants-awarded) (ok (var-get grants-awarded)))
(define-read-only (get-applicant-apps (who principal)) (ok (default-to u0 (map-get? applicant-history who))))
