;; Mental Health Contract
;; Mental health support community
;; Halal - caring for wellbeing
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var resource-count uint u0)
(define-data-var session-count uint u0)
(define-data-var total-funded uint u0)

(define-map counselors principal { name: (string-utf8 100), specialty: (string-utf8 100), sessions-given: uint, verified: bool })
(define-map support-resources uint { author: principal, title: (string-utf8 100), category: (string-ascii 20), content-hash: (string-ascii 64), views: uint, created: uint })
(define-map counseling-sessions uint { counselor: principal, session-type: (string-ascii 20), notes-hash: (string-ascii 64), block: uint })
(define-map community-fund-donors principal uint)

(define-public (register-counselor (name (string-utf8 100)) (specialty (string-utf8 100)))
  (begin (map-set counselors tx-sender { name: name, specialty: specialty, sessions-given: u0, verified: false }) (ok true)))

(define-public (verify-counselor (who principal))
  (let ((c (unwrap! (map-get? counselors who) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set counselors who (merge c { verified: true })) (ok true)))

(define-public (add-resource (title (string-utf8 100)) (category (string-ascii 20)) (content-hash (string-ascii 64)))
  (let ((id (+ (var-get resource-count) u1)))
    (map-set support-resources id { author: tx-sender, title: title, category: category, content-hash: content-hash, views: u0, created: stacks-block-height })
    (var-set resource-count id) (ok id)))

(define-public (view-resource (resource-id uint))
  (let ((r (unwrap! (map-get? support-resources resource-id) ERR-NOT-FOUND)))
    (map-set support-resources resource-id (merge r { views: (+ (get views r) u1) })) (ok true)))

(define-public (log-session (session-type (string-ascii 20)) (notes-hash (string-ascii 64)))
  (let (
    (c (unwrap! (map-get? counselors tx-sender) ERR-NOT-AUTHORIZED))
    (sid (+ (var-get session-count) u1))
  )
    (map-set counseling-sessions sid { counselor: tx-sender, session-type: session-type, notes-hash: notes-hash, block: stacks-block-height })
    (map-set counselors tx-sender (merge c { sessions-given: (+ (get sessions-given c) u1) }))
    (var-set session-count sid) (ok sid)))

(define-public (donate-to-fund (amount uint))
  (let ((prev (default-to u0 (map-get? community-fund-donors tx-sender))))
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set community-fund-donors tx-sender (+ prev amount))
    (var-set total-funded (+ (var-get total-funded) amount)) (ok amount)))

(define-read-only (get-counselor (who principal)) (map-get? counselors who))
(define-read-only (get-resource (id uint)) (map-get? support-resources id))
(define-read-only (get-session (id uint)) (map-get? counseling-sessions id))
(define-read-only (get-resource-count) (ok (var-get resource-count)))
(define-read-only (get-session-count) (ok (var-get session-count)))
(define-read-only (get-total-funded) (ok (var-get total-funded)))
