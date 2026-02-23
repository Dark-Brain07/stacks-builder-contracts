;; Skills Registry Contract
;; Register and verify professional skills
;; Halal - education and employment
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-ENDORSED (err u405))

(define-data-var skill-count uint u0)

(define-map user-skills { user: principal, index: uint } { name: (string-utf8 50), level: uint, endorsements: uint, added: uint })
(define-map user-skill-count principal uint)
(define-map endorsements { user: principal, skill-index: uint, endorser: principal } bool)
(define-map verifier-status principal bool)

(map-set verifier-status CONTRACT-OWNER true)

(define-public (add-skill (name (string-utf8 50)) (level uint))
  (let ((idx (default-to u0 (map-get? user-skill-count tx-sender))))
    (map-set user-skills { user: tx-sender, index: idx } { name: name, level: level, endorsements: u0, added: stacks-block-height })
    (map-set user-skill-count tx-sender (+ idx u1))
    (var-set skill-count (+ (var-get skill-count) u1))
    (ok idx)))

(define-public (endorse-skill (user principal) (skill-index uint))
  (let ((skill (unwrap! (map-get? user-skills { user: user, index: skill-index }) ERR-NOT-FOUND)))
    (asserts! (is-none (map-get? endorsements { user: user, skill-index: skill-index, endorser: tx-sender })) ERR-ALREADY-ENDORSED)
    (map-set endorsements { user: user, skill-index: skill-index, endorser: tx-sender } true)
    (map-set user-skills { user: user, index: skill-index } (merge skill { endorsements: (+ (get endorsements skill) u1) }))
    (ok true)))

(define-public (verify-skill (user principal) (skill-index uint) (new-level uint))
  (let ((skill (unwrap! (map-get? user-skills { user: user, index: skill-index }) ERR-NOT-FOUND)))
    (asserts! (default-to false (map-get? verifier-status tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set user-skills { user: user, index: skill-index } (merge skill { level: new-level }))
    (ok true)))

(define-public (add-verifier (v principal))
  (begin (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set verifier-status v true) (ok true)))

(define-read-only (get-skill (user principal) (index uint)) (map-get? user-skills { user: user, index: index }))
(define-read-only (get-skill-count-of (user principal)) (ok (default-to u0 (map-get? user-skill-count user))))
(define-read-only (get-total-skills) (ok (var-get skill-count)))
(define-read-only (is-endorsed (user principal) (skill-index uint) (endorser principal))
  (ok (default-to false (map-get? endorsements { user: user, skill-index: skill-index, endorser: endorser }))))
