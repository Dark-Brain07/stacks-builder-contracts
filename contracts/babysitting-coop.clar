;; Babysitting Coop Contract
;; Babysitting cooperative with time-credit system
;; Halal - community child care
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT (err u405))

(define-data-var member-count uint u0)
(define-data-var session-count uint u0)

(define-map members principal { name: (string-utf8 100), children: uint, credits: uint, sessions-given: uint, sessions-received: uint, joined: uint })
(define-map sessions uint {
  sitter: principal, parent: principal, children-count: uint,
  hours: uint, credits-exchanged: uint,
  status: (string-ascii 20), block: uint
})

(define-public (join-coop (name (string-utf8 100)) (children uint))
  (begin
    (map-set members tx-sender { name: name, children: children, credits: u10, sessions-given: u0, sessions-received: u0, joined: stacks-block-height })
    (var-set member-count (+ (var-get member-count) u1)) (ok true)))

(define-public (request-sitting (sitter principal) (children-count uint) (hours uint))
  (let (
    (parent (unwrap! (map-get? members tx-sender) ERR-NOT-FOUND))
    (s (unwrap! (map-get? members sitter) ERR-NOT-FOUND))
    (credits-needed hours)
    (sid (+ (var-get session-count) u1))
  )
    (asserts! (>= (get credits parent) credits-needed) ERR-INSUFFICIENT)
    (map-set sessions sid { sitter: sitter, parent: tx-sender, children-count: children-count, hours: hours, credits-exchanged: credits-needed, status: "requested", block: stacks-block-height })
    (var-set session-count sid) (ok sid)))

(define-public (accept-session (session-id uint))
  (let ((s (unwrap! (map-get? sessions session-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get sitter s)) ERR-NOT-AUTHORIZED)
    (map-set sessions session-id (merge s { status: "accepted" })) (ok true)))

(define-public (complete-session (session-id uint))
  (let (
    (s (unwrap! (map-get? sessions session-id) ERR-NOT-FOUND))
    (parent (unwrap! (map-get? members (get parent s)) ERR-NOT-FOUND))
    (sitter (unwrap! (map-get? members (get sitter s)) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get parent s)) ERR-NOT-AUTHORIZED)
    (map-set members (get parent s) (merge parent { credits: (- (get credits parent) (get credits-exchanged s)), sessions-received: (+ (get sessions-received parent) u1) }))
    (map-set members (get sitter s) (merge sitter { credits: (+ (get credits sitter) (get credits-exchanged s)), sessions-given: (+ (get sessions-given sitter) u1) }))
    (map-set sessions session-id (merge s { status: "completed" })) (ok true)))

(define-public (cancel-session (session-id uint))
  (let ((s (unwrap! (map-get? sessions session-id) ERR-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender (get parent s)) (is-eq tx-sender (get sitter s))) ERR-NOT-AUTHORIZED)
    (map-set sessions session-id (merge s { status: "cancelled" })) (ok true)))

(define-read-only (get-member (who principal)) (map-get? members who))
(define-read-only (get-session (id uint)) (map-get? sessions id))
(define-read-only (get-member-count) (ok (var-get member-count)))
(define-read-only (get-session-count) (ok (var-get session-count)))
