;; Dispute Resolver Contract
;; On-chain arbitration and dispute resolution
;; Halal - justice (adl)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-RESOLVED (err u405))
(define-constant ERR-NOT-PARTY (err u406))
(define-constant ERR-NOT-ARBITER (err u407))

(define-data-var dispute-count uint u0)
(define-data-var resolved-count uint u0)

(define-map arbiters principal { cases-handled: uint, active: bool })
(define-map disputes uint {
  claimant: principal, respondent: principal, arbiter: (optional principal),
  description: (string-utf8 200), stake: uint, status: (string-ascii 20),
  ruling: (string-utf8 200), created: uint
})

(map-set arbiters CONTRACT-OWNER { cases-handled: u0, active: true })

(define-public (register-arbiter)
  (begin (map-set arbiters tx-sender { cases-handled: u0, active: true }) (ok true)))

(define-public (file-dispute (respondent principal) (description (string-utf8 200)) (stake uint))
  (let ((id (+ (var-get dispute-count) u1)))
    (try! (stx-transfer? stake tx-sender CONTRACT-OWNER))
    (map-set disputes id {
      claimant: tx-sender, respondent: respondent, arbiter: none,
      description: description, stake: stake, status: "filed", ruling: u"", created: stacks-block-height
    })
    (var-set dispute-count id) (ok id)))

(define-public (accept-case (dispute-id uint))
  (let (
    (dispute (unwrap! (map-get? disputes dispute-id) ERR-NOT-FOUND))
    (arb (unwrap! (map-get? arbiters tx-sender) ERR-NOT-ARBITER))
  )
    (asserts! (get active arb) ERR-NOT-ARBITER)
    (map-set disputes dispute-id (merge dispute { arbiter: (some tx-sender), status: "in-review" }))
    (ok true)))

(define-public (make-ruling (dispute-id uint) (ruling (string-utf8 200)) (award-to-claimant bool))
  (let ((dispute (unwrap! (map-get? disputes dispute-id) ERR-NOT-FOUND)))
    (asserts! (match (get arbiter dispute) a (is-eq tx-sender a) false) ERR-NOT-ARBITER)
    (asserts! (not (is-eq (get status dispute) "resolved")) ERR-ALREADY-RESOLVED)
    (if award-to-claimant
      (try! (stx-transfer? (get stake dispute) CONTRACT-OWNER (get claimant dispute)))
      (try! (stx-transfer? (get stake dispute) CONTRACT-OWNER (get respondent dispute))))
    (map-set disputes dispute-id (merge dispute { status: "resolved", ruling: ruling }))
    (match (get arbiter dispute) a
      (map-set arbiters a (merge (default-to { cases-handled: u0, active: true } (map-get? arbiters a)) { cases-handled: (+ (get cases-handled (default-to { cases-handled: u0, active: true } (map-get? arbiters a))) u1) }))
      true)
    (var-set resolved-count (+ (var-get resolved-count) u1))
    (ok true)))

(define-read-only (get-dispute (id uint)) (map-get? disputes id))
(define-read-only (get-arbiter (who principal)) (map-get? arbiters who))
(define-read-only (get-dispute-count) (ok (var-get dispute-count)))
(define-read-only (get-resolved-count) (ok (var-get resolved-count)))
