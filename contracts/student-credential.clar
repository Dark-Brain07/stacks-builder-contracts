;; Student Credential Contract
;; On-chain student transcripts and credentials
;; Halal - education verification
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-NOT-INSTITUTION (err u405))

(define-data-var credential-count uint u0)

(define-map institutions principal { name: (string-utf8 100), verified: bool, credentials-issued: uint })
(define-map credentials uint {
  student: principal, institution: principal, credential-type: (string-ascii 30),
  title: (string-utf8 100), grade: (string-utf8 20), issued: uint, revoked: bool
})
(define-map student-records principal { total-credentials: uint })

(define-public (register-institution (name (string-utf8 100)))
  (begin (map-set institutions tx-sender { name: name, verified: false, credentials-issued: u0 }) (ok true)))

(define-public (verify-institution (inst principal))
  (let ((i (unwrap! (map-get? institutions inst) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set institutions inst (merge i { verified: true })) (ok true)))

(define-public (issue-credential (student principal) (cred-type (string-ascii 30)) (title (string-utf8 100)) (grade (string-utf8 20)))
  (let (
    (inst (unwrap! (map-get? institutions tx-sender) ERR-NOT-INSTITUTION))
    (id (+ (var-get credential-count) u1))
    (student-rec (default-to { total-credentials: u0 } (map-get? student-records student)))
  )
    (asserts! (get verified inst) ERR-NOT-INSTITUTION)
    (map-set credentials id { student: student, institution: tx-sender, credential-type: cred-type, title: title, grade: grade, issued: stacks-block-height, revoked: false })
    (map-set institutions tx-sender (merge inst { credentials-issued: (+ (get credentials-issued inst) u1) }))
    (map-set student-records student { total-credentials: (+ (get total-credentials student-rec) u1) })
    (var-set credential-count id)
    (print { event: "credential-issued", id: id, student: student })
    (ok id)))

(define-public (revoke-credential (cred-id uint))
  (let ((cred (unwrap! (map-get? credentials cred-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get institution cred)) ERR-NOT-AUTHORIZED)
    (map-set credentials cred-id (merge cred { revoked: true })) (ok true)))

(define-read-only (get-credential (id uint)) (map-get? credentials id))
(define-read-only (get-institution (who principal)) (map-get? institutions who))
(define-read-only (get-student-records (who principal)) (map-get? student-records who))
(define-read-only (get-credential-count) (ok (var-get credential-count)))
(define-read-only (is-valid-credential (id uint))
  (match (map-get? credentials id) c (ok (not (get revoked c))) (ok false)))
