;; Compliance Checker Contract
;; Shariah compliance verification system
;; Halal - ensures Islamic compliance
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-NOT-SCHOLAR (err u405))

(define-data-var review-count uint u0)
(define-data-var approved-count uint u0)

(define-map scholars principal { name: (string-utf8 100), specialization: (string-utf8 100), reviews-done: uint, active: bool })
(define-map compliance-reviews uint {
  submitter: principal, project-name: (string-utf8 100), category: (string-ascii 30),
  description: (string-utf8 200), reviewer: (optional principal),
  status: (string-ascii 20), ruling: (string-utf8 200), submitted: uint
})
(define-map project-status (string-utf8 100) (string-ascii 20))

(define-public (register-scholar (name (string-utf8 100)) (spec (string-utf8 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set scholars tx-sender { name: name, specialization: spec, reviews-done: u0, active: true })
    (ok true)))

(define-public (add-scholar (scholar principal) (name (string-utf8 100)) (spec (string-utf8 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set scholars scholar { name: name, specialization: spec, reviews-done: u0, active: true }) (ok true)))

(define-public (submit-for-review (project-name (string-utf8 100)) (category (string-ascii 30)) (description (string-utf8 200)))
  (let ((id (+ (var-get review-count) u1)))
    (map-set compliance-reviews id { submitter: tx-sender, project-name: project-name, category: category, description: description, reviewer: none, status: "pending", ruling: u"", submitted: stacks-block-height })
    (var-set review-count id) (ok id)))

(define-public (accept-review (review-id uint))
  (let (
    (review (unwrap! (map-get? compliance-reviews review-id) ERR-NOT-FOUND))
    (scholar (unwrap! (map-get? scholars tx-sender) ERR-NOT-SCHOLAR))
  )
    (asserts! (get active scholar) ERR-NOT-SCHOLAR)
    (map-set compliance-reviews review-id (merge review { reviewer: (some tx-sender), status: "in-review" }))
    (ok true)))

(define-public (issue-ruling (review-id uint) (is-compliant bool) (ruling (string-utf8 200)))
  (let (
    (review (unwrap! (map-get? compliance-reviews review-id) ERR-NOT-FOUND))
    (scholar (unwrap! (map-get? scholars tx-sender) ERR-NOT-SCHOLAR))
  )
    (asserts! (match (get reviewer review) r (is-eq tx-sender r) false) ERR-NOT-SCHOLAR)
    (map-set compliance-reviews review-id (merge review { status: (if is-compliant "halal" "haram"), ruling: ruling }))
    (map-set project-status (get project-name review) (if is-compliant "halal" "haram"))
    (map-set scholars tx-sender (merge scholar { reviews-done: (+ (get reviews-done scholar) u1) }))
    (if is-compliant (var-set approved-count (+ (var-get approved-count) u1)) true)
    (ok is-compliant)))

(define-read-only (get-review (id uint)) (map-get? compliance-reviews id))
(define-read-only (get-scholar (who principal)) (map-get? scholars who))
(define-read-only (get-project-compliance (name (string-utf8 100))) (map-get? project-status name))
(define-read-only (get-review-count) (ok (var-get review-count)))
(define-read-only (get-approved-count) (ok (var-get approved-count)))
