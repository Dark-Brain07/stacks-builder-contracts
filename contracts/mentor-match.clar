;; Mentor Match Contract
;; Connect mentors with mentees
;; Halal - knowledge transfer
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-REGISTERED (err u405))

(define-data-var mentor-count uint u0)
(define-data-var match-count uint u0)
(define-data-var session-count uint u0)

(define-map mentors principal { name: (string-utf8 50), expertise: (string-utf8 100), rate-per-session: uint, sessions-given: uint, rating-total: uint, ratings: uint, active: bool })
(define-map mentees principal { name: (string-utf8 50), interests: (string-utf8 100), sessions-taken: uint })
(define-map sessions uint { mentor: principal, mentee: principal, topic: (string-utf8 100), completed: bool, rating: uint, block: uint })

(define-public (register-mentor (name (string-utf8 50)) (expertise (string-utf8 100)) (rate uint))
  (begin
    (asserts! (is-none (map-get? mentors tx-sender)) ERR-ALREADY-REGISTERED)
    (map-set mentors tx-sender { name: name, expertise: expertise, rate-per-session: rate, sessions-given: u0, rating-total: u0, ratings: u0, active: true })
    (var-set mentor-count (+ (var-get mentor-count) u1)) (ok true)))

(define-public (register-mentee (name (string-utf8 50)) (interests (string-utf8 100)))
  (begin
    (map-set mentees tx-sender { name: name, interests: interests, sessions-taken: u0 }) (ok true)))

(define-public (book-session (mentor principal) (topic (string-utf8 100)))
  (let (
    (m (unwrap! (map-get? mentors mentor) ERR-NOT-FOUND))
    (id (+ (var-get session-count) u1))
  )
    (asserts! (get active m) ERR-NOT-FOUND)
    (if (> (get rate-per-session m) u0)
      (try! (stx-transfer? (get rate-per-session m) tx-sender mentor)) true)
    (map-set sessions id { mentor: mentor, mentee: tx-sender, topic: topic, completed: false, rating: u0, block: stacks-block-height })
    (var-set session-count id)
    (var-set match-count (+ (var-get match-count) u1))
    (ok id)))

(define-public (complete-session (session-id uint) (rating uint))
  (let (
    (session (unwrap! (map-get? sessions session-id) ERR-NOT-FOUND))
    (m (unwrap! (map-get? mentors (get mentor session)) ERR-NOT-FOUND))
    (mentee (default-to { name: u"", interests: u"", sessions-taken: u0 } (map-get? mentees (get mentee session))))
  )
    (asserts! (is-eq tx-sender (get mentee session)) ERR-NOT-AUTHORIZED)
    (map-set sessions session-id (merge session { completed: true, rating: rating }))
    (map-set mentors (get mentor session) (merge m { sessions-given: (+ (get sessions-given m) u1), rating-total: (+ (get rating-total m) rating), ratings: (+ (get ratings m) u1) }))
    (map-set mentees (get mentee session) (merge mentee { sessions-taken: (+ (get sessions-taken mentee) u1) }))
    (ok true)))

(define-read-only (get-mentor (who principal)) (map-get? mentors who))
(define-read-only (get-mentee (who principal)) (map-get? mentees who))
(define-read-only (get-session (id uint)) (map-get? sessions id))
(define-read-only (get-mentor-count) (ok (var-get mentor-count)))
(define-read-only (get-match-count) (ok (var-get match-count)))
(define-read-only (get-mentor-rating (who principal))
  (match (map-get? mentors who) m (if (> (get ratings m) u0) (ok (/ (get rating-total m) (get ratings m))) (ok u0)) (ok u0)))
