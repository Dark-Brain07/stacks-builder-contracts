;; Talent Pool Contract
;; Talent recruitment and job matching
;; Halal - fair employment
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-EXISTS (err u405))

(define-data-var talent-count uint u0)
(define-data-var position-count uint u0)
(define-data-var match-count uint u0)

(define-map talents principal {
  name: (string-utf8 100), skills: (string-utf8 200), experience-years: uint,
  available: bool, hourly-rate: uint, endorsements: uint, registered: uint
})
(define-map positions uint {
  company: principal, title: (string-utf8 100), requirements: (string-utf8 200),
  budget: uint, applicants: uint, filled: bool, posted: uint
})
(define-map applications { position-id: uint, talent: principal } { applied: uint, status: (string-ascii 20) })
(define-map endorsements { talent: principal, endorser: principal } { skill: (string-utf8 50), block: uint })

(define-public (register-talent (name (string-utf8 100)) (skills (string-utf8 200)) (experience uint) (rate uint))
  (begin
    (asserts! (is-none (map-get? talents tx-sender)) ERR-ALREADY-EXISTS)
    (map-set talents tx-sender { name: name, skills: skills, experience-years: experience, available: true, hourly-rate: rate, endorsements: u0, registered: stacks-block-height })
    (var-set talent-count (+ (var-get talent-count) u1)) (ok true)))

(define-public (post-position (title (string-utf8 100)) (requirements (string-utf8 200)) (budget uint))
  (let ((id (+ (var-get position-count) u1)))
    (map-set positions id { company: tx-sender, title: title, requirements: requirements, budget: budget, applicants: u0, filled: false, posted: stacks-block-height })
    (var-set position-count id) (ok id)))

(define-public (apply-position (position-id uint))
  (let ((pos (unwrap! (map-get? positions position-id) ERR-NOT-FOUND)))
    (asserts! (not (get filled pos)) ERR-NOT-FOUND)
    (asserts! (is-some (map-get? talents tx-sender)) ERR-NOT-FOUND)
    (map-set applications { position-id: position-id, talent: tx-sender } { applied: stacks-block-height, status: "applied" })
    (map-set positions position-id (merge pos { applicants: (+ (get applicants pos) u1) }))
    (ok true)))

(define-public (hire-talent (position-id uint) (talent principal))
  (let (
    (pos (unwrap! (map-get? positions position-id) ERR-NOT-FOUND))
    (app (unwrap! (map-get? applications { position-id: position-id, talent: talent }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get company pos)) ERR-NOT-AUTHORIZED)
    (map-set applications { position-id: position-id, talent: talent } (merge app { status: "hired" }))
    (map-set positions position-id (merge pos { filled: true }))
    (var-set match-count (+ (var-get match-count) u1))
    (ok true)))

(define-public (endorse-talent (talent principal) (skill (string-utf8 50)))
  (let ((t (unwrap! (map-get? talents talent) ERR-NOT-FOUND)))
    (map-set endorsements { talent: talent, endorser: tx-sender } { skill: skill, block: stacks-block-height })
    (map-set talents talent (merge t { endorsements: (+ (get endorsements t) u1) }))
    (ok true)))

(define-read-only (get-talent (who principal)) (map-get? talents who))
(define-read-only (get-position (id uint)) (map-get? positions id))
(define-read-only (get-application (position-id uint) (talent principal)) (map-get? applications { position-id: position-id, talent: talent }))
(define-read-only (get-talent-count) (ok (var-get talent-count)))
(define-read-only (get-position-count) (ok (var-get position-count)))
(define-read-only (get-match-count) (ok (var-get match-count)))
