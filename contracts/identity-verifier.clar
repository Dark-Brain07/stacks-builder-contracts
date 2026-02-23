;; Identity Verifier Contract
;; On-chain identity/KYC verification
;; Halal - trust and verification
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-ALREADY-VERIFIED (err u402))
(define-constant ERR-NOT-VERIFIER (err u403))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var verified-count uint u0)

(define-map verifiers principal bool)
(define-map identities principal { level: uint, verified-by: principal, verified-at: uint, revoked: bool })
(define-map verification-requests principal { requested-at: uint, status: (string-ascii 20) })

(map-set verifiers CONTRACT-OWNER true)

(define-public (add-verifier (v principal))
  (begin (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set verifiers v true) (ok true)))

(define-public (request-verification)
  (begin
    (map-set verification-requests tx-sender { requested-at: stacks-block-height, status: "pending" })
    (ok true)))

(define-public (verify-identity (user principal) (level uint))
  (begin
    (asserts! (default-to false (map-get? verifiers tx-sender)) ERR-NOT-VERIFIER)
    (asserts! (is-none (map-get? identities user)) ERR-ALREADY-VERIFIED)
    (map-set identities user { level: level, verified-by: tx-sender, verified-at: stacks-block-height, revoked: false })
    (map-set verification-requests user { requested-at: stacks-block-height, status: "approved" })
    (var-set verified-count (+ (var-get verified-count) u1))
    (ok true)))

(define-public (revoke-identity (user principal))
  (let ((id (unwrap! (map-get? identities user) ERR-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender (get verified-by id))) ERR-NOT-AUTHORIZED)
    (map-set identities user (merge id { revoked: true })) (ok true)))

(define-public (upgrade-level (user principal) (new-level uint))
  (let ((id (unwrap! (map-get? identities user) ERR-NOT-FOUND)))
    (asserts! (default-to false (map-get? verifiers tx-sender)) ERR-NOT-VERIFIER)
    (map-set identities user (merge id { level: new-level })) (ok true)))

(define-read-only (get-identity (user principal)) (map-get? identities user))
(define-read-only (get-verification-status (user principal)) (map-get? verification-requests user))
(define-read-only (is-verified (user principal))
  (match (map-get? identities user) id (ok (and (not (get revoked id)) (> (get level id) u0))) (ok false)))
(define-read-only (get-verified-count) (ok (var-get verified-count)))
