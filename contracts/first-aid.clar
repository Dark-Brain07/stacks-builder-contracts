;; First Aid Contract
;; First aid training and certification
;; Halal - saving lives
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var course-count uint u0)
(define-data-var cert-count uint u0)

(define-map courses uint { instructor: principal, title: (string-utf8 100), level: (string-ascii 20), max-students: uint, enrolled: uint, fee: uint, active: bool })
(define-map enrollments { course-id: uint, student: principal } { paid: uint, completed: bool, enrolled: uint })
(define-map certificates uint { student: principal, course-id: uint, instructor: principal, issued: uint, valid-until: uint })
(define-map instructor-profiles principal { name: (string-utf8 100), certifications: (string-utf8 200), students-trained: uint })

(define-public (register-instructor (name (string-utf8 100)) (certifications (string-utf8 200)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set instructor-profiles tx-sender { name: name, certifications: certifications, students-trained: u0 }) (ok true)))

(define-public (add-instructor (who principal) (name (string-utf8 100)) (certifications (string-utf8 200)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set instructor-profiles who { name: name, certifications: certifications, students-trained: u0 }) (ok true)))

(define-public (create-course (title (string-utf8 100)) (level (string-ascii 20)) (max-students uint) (fee uint))
  (let ((id (+ (var-get course-count) u1)))
    (asserts! (is-some (map-get? instructor-profiles tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set courses id { instructor: tx-sender, title: title, level: level, max-students: max-students, enrolled: u0, fee: fee, active: true })
    (var-set course-count id) (ok id)))

(define-public (enroll (course-id uint))
  (let ((c (unwrap! (map-get? courses course-id) ERR-NOT-FOUND)))
    (asserts! (get active c) ERR-NOT-FOUND)
    (try! (stx-transfer? (get fee c) tx-sender (get instructor c)))
    (map-set enrollments { course-id: course-id, student: tx-sender } { paid: (get fee c), completed: false, enrolled: stacks-block-height })
    (map-set courses course-id (merge c { enrolled: (+ (get enrolled c) u1) })) (ok true)))

(define-public (issue-certificate (course-id uint) (student principal) (valid-blocks uint))
  (let (
    (c (unwrap! (map-get? courses course-id) ERR-NOT-FOUND))
    (cid (+ (var-get cert-count) u1))
    (inst (unwrap! (map-get? instructor-profiles tx-sender) ERR-NOT-AUTHORIZED))
  )
    (asserts! (is-eq tx-sender (get instructor c)) ERR-NOT-AUTHORIZED)
    (map-set enrollments { course-id: course-id, student: student } { paid: u0, completed: true, enrolled: stacks-block-height })
    (map-set certificates cid { student: student, course-id: course-id, instructor: tx-sender, issued: stacks-block-height, valid-until: (+ stacks-block-height valid-blocks) })
    (map-set instructor-profiles tx-sender (merge inst { students-trained: (+ (get students-trained inst) u1) }))
    (var-set cert-count cid) (ok cid)))

(define-read-only (get-course (id uint)) (map-get? courses id))
(define-read-only (get-enrollment (course-id uint) (student principal)) (map-get? enrollments { course-id: course-id, student: student }))
(define-read-only (get-certificate (id uint)) (map-get? certificates id))
(define-read-only (get-instructor (who principal)) (map-get? instructor-profiles who))
(define-read-only (get-course-count) (ok (var-get course-count)))
(define-read-only (get-cert-count) (ok (var-get cert-count)))
