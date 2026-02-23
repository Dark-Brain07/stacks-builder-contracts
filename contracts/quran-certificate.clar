;; Quran Certificate Contract
;; Quran memorization milestone certificates
;; Halal - encouraging Quran learning
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-NOT-TEACHER (err u405))

(define-data-var cert-count uint u0)
(define-data-var student-count uint u0)

(define-map teachers principal { name: (string-utf8 100), institution: (string-utf8 100), certs-issued: uint, verified: bool })
(define-map students-reg principal { name: (string-utf8 100), teacher: principal, juz-completed: uint, enrolled: uint })
(define-map certificates uint {
  student: principal, teacher: principal, milestone: (string-utf8 100),
  juz-from: uint, juz-to: uint, grade: (string-ascii 10),
  cert-hash: (buff 32), issued: uint
})
(define-map student-certs { student: principal, index: uint } uint)
(define-map student-cert-count principal uint)

(define-public (register-teacher (name (string-utf8 100)) (institution (string-utf8 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set teachers tx-sender { name: name, institution: institution, certs-issued: u0, verified: true }) (ok true)))

(define-public (add-teacher (teacher principal) (name (string-utf8 100)) (institution (string-utf8 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set teachers teacher { name: name, institution: institution, certs-issued: u0, verified: true }) (ok true)))

(define-public (enroll-student (name (string-utf8 100)))
  (begin
    (map-set students-reg tx-sender { name: name, teacher: CONTRACT-OWNER, juz-completed: u0, enrolled: stacks-block-height })
    (var-set student-count (+ (var-get student-count) u1)) (ok true)))

(define-public (issue-certificate (student principal) (milestone (string-utf8 100)) (juz-from uint) (juz-to uint) (grade (string-ascii 10)) (cert-hash (buff 32)))
  (let (
    (teacher (unwrap! (map-get? teachers tx-sender) ERR-NOT-TEACHER))
    (id (+ (var-get cert-count) u1))
    (sc (default-to u0 (map-get? student-cert-count student)))
  )
    (asserts! (get verified teacher) ERR-NOT-TEACHER)
    (map-set certificates id { student: student, teacher: tx-sender, milestone: milestone, juz-from: juz-from, juz-to: juz-to, grade: grade, cert-hash: cert-hash, issued: stacks-block-height })
    (map-set student-certs { student: student, index: sc } id)
    (map-set student-cert-count student (+ sc u1))
    (map-set teachers tx-sender (merge teacher { certs-issued: (+ (get certs-issued teacher) u1) }))
    (match (map-get? students-reg student)
      s (map-set students-reg student (merge s { juz-completed: juz-to })) true)
    (var-set cert-count id) (ok id)))

(define-read-only (get-certificate (id uint)) (map-get? certificates id))
(define-read-only (get-teacher (who principal)) (map-get? teachers who))
(define-read-only (get-student (who principal)) (map-get? students-reg who))
(define-read-only (get-student-cert (student principal) (index uint)) (map-get? student-certs { student: student, index: index }))
(define-read-only (get-cert-count) (ok (var-get cert-count)))
(define-read-only (get-student-count) (ok (var-get student-count)))
