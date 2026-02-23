;; Tutoring Market Contract
;; Peer-to-peer tutoring marketplace
;; Halal - knowledge sharing
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant PLATFORM-FEE-PCT u3)

(define-data-var tutor-count uint u0)
(define-data-var session-count uint u0)
(define-data-var total-revenue uint u0)

(define-map tutors principal { name: (string-utf8 100), subjects: (string-utf8 200), rate: uint, sessions: uint, rating-total: uint, ratings: uint, active: bool })
(define-map sessions uint { tutor: principal, student: principal, subject: (string-utf8 100), duration-blocks: uint, fee: uint, status: (string-ascii 20), booked: uint })

(define-public (register-tutor (name (string-utf8 100)) (subjects (string-utf8 200)) (rate uint))
  (begin
    (map-set tutors tx-sender { name: name, subjects: subjects, rate: rate, sessions: u0, rating-total: u0, ratings: u0, active: true })
    (var-set tutor-count (+ (var-get tutor-count) u1)) (ok true)))

(define-public (book-session (tutor principal) (subject (string-utf8 100)) (duration uint))
  (let (
    (t (unwrap! (map-get? tutors tutor) ERR-NOT-FOUND))
    (fee (* (get rate t) duration))
    (platform-fee (/ (* fee PLATFORM-FEE-PCT) u100))
    (tutor-pay (- fee platform-fee))
    (sid (+ (var-get session-count) u1))
  )
    (asserts! (get active t) ERR-NOT-FOUND)
    (try! (stx-transfer? tutor-pay tx-sender tutor))
    (try! (stx-transfer? platform-fee tx-sender CONTRACT-OWNER))
    (map-set sessions sid { tutor: tutor, student: tx-sender, subject: subject, duration-blocks: duration, fee: fee, status: "booked", booked: stacks-block-height })
    (map-set tutors tutor (merge t { sessions: (+ (get sessions t) u1) }))
    (var-set session-count sid)
    (var-set total-revenue (+ (var-get total-revenue) fee))
    (ok sid)))

(define-public (complete-session (session-id uint))
  (let ((s (unwrap! (map-get? sessions session-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get tutor s)) ERR-NOT-AUTHORIZED)
    (map-set sessions session-id (merge s { status: "completed" })) (ok true)))

(define-public (rate-session (session-id uint) (rating uint))
  (let (
    (s (unwrap! (map-get? sessions session-id) ERR-NOT-FOUND))
    (t (unwrap! (map-get? tutors (get tutor s)) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get student s)) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-NOT-AUTHORIZED)
    (map-set tutors (get tutor s) (merge t { rating-total: (+ (get rating-total t) rating), ratings: (+ (get ratings t) u1) }))
    (ok rating)))

(define-public (update-rate (new-rate uint))
  (let ((t (unwrap! (map-get? tutors tx-sender) ERR-NOT-FOUND)))
    (map-set tutors tx-sender (merge t { rate: new-rate })) (ok new-rate)))

(define-read-only (get-tutor (who principal)) (map-get? tutors who))
(define-read-only (get-session (id uint)) (map-get? sessions id))
(define-read-only (get-tutor-count) (ok (var-get tutor-count)))
(define-read-only (get-session-count) (ok (var-get session-count)))
(define-read-only (get-total-revenue) (ok (var-get total-revenue)))
