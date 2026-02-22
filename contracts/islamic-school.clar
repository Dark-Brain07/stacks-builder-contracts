;; Islamic School Contract
;; Madrasah enrollment, fees and course management
;; Halal - Islamic education
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-ENROLLED (err u405))

(define-data-var course-count uint u0)
(define-data-var enrollment-count uint u0)
(define-data-var total-fees uint u0)

(define-map courses uint { name: (string-utf8 100), instructor: principal, fee: uint, capacity: uint, enrolled: uint, active: bool })
(define-map enrollments { course-id: uint, student: principal } { paid: uint, grade: (string-ascii 5), completed: bool, enrolled-at: uint })
(define-map instructors principal { name: (string-utf8 100), courses-taught: uint, students-total: uint, active: bool })

(define-public (register-instructor (name (string-utf8 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set instructors tx-sender { name: name, courses-taught: u0, students-total: u0, active: true }) (ok true)))

(define-public (add-instructor (instr principal) (name (string-utf8 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set instructors instr { name: name, courses-taught: u0, students-total: u0, active: true }) (ok true)))

(define-public (create-course (name (string-utf8 100)) (fee uint) (capacity uint))
  (let (
    (id (+ (var-get course-count) u1))
    (instr (unwrap! (map-get? instructors tx-sender) ERR-NOT-AUTHORIZED))
  )
    (map-set courses id { name: name, instructor: tx-sender, fee: fee, capacity: capacity, enrolled: u0, active: true })
    (map-set instructors tx-sender (merge instr { courses-taught: (+ (get courses-taught instr) u1) }))
    (var-set course-count id) (ok id)))

(define-public (enroll (course-id uint))
  (let (
    (course (unwrap! (map-get? courses course-id) ERR-NOT-FOUND))
    (instr (unwrap! (map-get? instructors (get instructor course)) ERR-NOT-FOUND))
  )
    (asserts! (get active course) ERR-NOT-FOUND)
    (asserts! (< (get enrolled course) (get capacity course)) ERR-NOT-FOUND)
    (asserts! (is-none (map-get? enrollments { course-id: course-id, student: tx-sender })) ERR-ALREADY-ENROLLED)
    (try! (stx-transfer? (get fee course) tx-sender (get instructor course)))
    (map-set enrollments { course-id: course-id, student: tx-sender } { paid: (get fee course), grade: "N/A", completed: false, enrolled-at: stacks-block-height })
    (map-set courses course-id (merge course { enrolled: (+ (get enrolled course) u1) }))
    (map-set instructors (get instructor course) (merge instr { students-total: (+ (get students-total instr) u1) }))
    (var-set enrollment-count (+ (var-get enrollment-count) u1))
    (var-set total-fees (+ (var-get total-fees) (get fee course)))
    (ok true)))

(define-public (assign-grade (course-id uint) (student principal) (grade (string-ascii 5)))
  (let (
    (course (unwrap! (map-get? courses course-id) ERR-NOT-FOUND))
    (enr (unwrap! (map-get? enrollments { course-id: course-id, student: student }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get instructor course)) ERR-NOT-AUTHORIZED)
    (map-set enrollments { course-id: course-id, student: student } (merge enr { grade: grade, completed: true }))
    (ok true)))

(define-read-only (get-course (id uint)) (map-get? courses id))
(define-read-only (get-enrollment (course-id uint) (student principal)) (map-get? enrollments { course-id: course-id, student: student }))
(define-read-only (get-instructor (who principal)) (map-get? instructors who))
(define-read-only (get-course-count) (ok (var-get course-count)))
(define-read-only (get-enrollment-count) (ok (var-get enrollment-count)))
(define-read-only (get-total-fees) (ok (var-get total-fees)))
