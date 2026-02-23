;; Job Board Contract
;; Post jobs and apply on-chain
;; Halal - fair employment
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-APPLIED (err u405))
(define-constant ERR-CLOSED (err u406))
(define-constant POSTING-FEE u500000) ;; 0.5 STX

(define-data-var job-count uint u0)
(define-data-var application-count uint u0)

(define-map jobs uint {
  employer: principal, title: (string-utf8 100), description: (string-utf8 200),
  salary-range: (string-utf8 50), job-type: (string-ascii 20),
  applications: uint, status: (string-ascii 20), posted: uint
})
(define-map applications uint { job-id: uint, applicant: principal, cover: (string-utf8 200), status: (string-ascii 20), applied: uint })
(define-map has-applied { job-id: uint, applicant: principal } uint)

(define-public (post-job (title (string-utf8 100)) (description (string-utf8 200)) (salary (string-utf8 50)) (job-type (string-ascii 20)))
  (let ((id (+ (var-get job-count) u1)))
    (try! (stx-transfer? POSTING-FEE tx-sender CONTRACT-OWNER))
    (map-set jobs id {
      employer: tx-sender, title: title, description: description,
      salary-range: salary, job-type: job-type, applications: u0, status: "open", posted: stacks-block-height
    })
    (var-set job-count id) (ok id)))

(define-public (apply-for-job (job-id uint) (cover (string-utf8 200)))
  (let (
    (job (unwrap! (map-get? jobs job-id) ERR-NOT-FOUND))
    (aid (+ (var-get application-count) u1))
  )
    (asserts! (is-eq (get status job) "open") ERR-CLOSED)
    (asserts! (is-none (map-get? has-applied { job-id: job-id, applicant: tx-sender })) ERR-ALREADY-APPLIED)
    (map-set applications aid { job-id: job-id, applicant: tx-sender, cover: cover, status: "pending", applied: stacks-block-height })
    (map-set has-applied { job-id: job-id, applicant: tx-sender } aid)
    (map-set jobs job-id (merge job { applications: (+ (get applications job) u1) }))
    (var-set application-count aid) (ok aid)))

(define-public (update-application-status (app-id uint) (new-status (string-ascii 20)))
  (let (
    (app (unwrap! (map-get? applications app-id) ERR-NOT-FOUND))
    (job (unwrap! (map-get? jobs (get job-id app)) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get employer job)) ERR-NOT-AUTHORIZED)
    (map-set applications app-id (merge app { status: new-status })) (ok true)))

(define-public (close-job (job-id uint))
  (let ((job (unwrap! (map-get? jobs job-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get employer job)) ERR-NOT-AUTHORIZED)
    (map-set jobs job-id (merge job { status: "closed" })) (ok true)))

(define-read-only (get-job (id uint)) (map-get? jobs id))
(define-read-only (get-application (id uint)) (map-get? applications id))
(define-read-only (get-job-count) (ok (var-get job-count)))
(define-read-only (get-application-count) (ok (var-get application-count)))
