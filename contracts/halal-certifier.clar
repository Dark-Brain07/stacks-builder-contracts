;; Halal Certifier Contract
;; Halal food/product certification system
;; Halal - ensuring product compliance
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-NOT-INSPECTOR (err u405))
(define-constant ERR-ALREADY-CERTIFIED (err u406))

(define-data-var cert-count uint u0)
(define-data-var total-certified uint u0)

(define-map inspectors principal { name: (string-utf8 100), authority: (string-utf8 100), inspections: uint, active: bool })
(define-map certifications uint {
  product: (string-utf8 100), producer: principal, inspector: principal,
  category: (string-ascii 20), cert-hash: (buff 32),
  valid-until: uint, status: (string-ascii 20), issued: uint
})
(define-map producer-certs principal uint)

(define-public (register-inspector (name (string-utf8 100)) (authority (string-utf8 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set inspectors tx-sender { name: name, authority: authority, inspections: u0, active: true }) (ok true)))

(define-public (add-inspector (inspector principal) (name (string-utf8 100)) (authority (string-utf8 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set inspectors inspector { name: name, authority: authority, inspections: u0, active: true }) (ok true)))

(define-public (certify-product (product (string-utf8 100)) (producer principal) (category (string-ascii 20)) (cert-hash (buff 32)) (validity uint))
  (let (
    (insp (unwrap! (map-get? inspectors tx-sender) ERR-NOT-INSPECTOR))
    (id (+ (var-get cert-count) u1))
  )
    (asserts! (get active insp) ERR-NOT-INSPECTOR)
    (map-set certifications id { product: product, producer: producer, inspector: tx-sender, category: category, cert-hash: cert-hash, valid-until: (+ stacks-block-height validity), status: "active", issued: stacks-block-height })
    (map-set inspectors tx-sender (merge insp { inspections: (+ (get inspections insp) u1) }))
    (map-set producer-certs producer (+ (default-to u0 (map-get? producer-certs producer)) u1))
    (var-set cert-count id)
    (var-set total-certified (+ (var-get total-certified) u1))
    (ok id)))

(define-public (revoke-certification (cert-id uint))
  (let ((cert (unwrap! (map-get? certifications cert-id) ERR-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender (get inspector cert)) (is-eq tx-sender CONTRACT-OWNER)) ERR-NOT-AUTHORIZED)
    (map-set certifications cert-id (merge cert { status: "revoked" })) (ok true)))

(define-public (renew-certification (cert-id uint) (validity uint))
  (let ((cert (unwrap! (map-get? certifications cert-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get inspector cert)) ERR-NOT-AUTHORIZED)
    (map-set certifications cert-id (merge cert { valid-until: (+ stacks-block-height validity), status: "active" })) (ok true)))

(define-read-only (get-certification (id uint)) (map-get? certifications id))
(define-read-only (get-inspector (who principal)) (map-get? inspectors who))
(define-read-only (get-cert-count) (ok (var-get cert-count)))
(define-read-only (get-total-certified) (ok (var-get total-certified)))
(define-read-only (is-valid-cert (id uint))
  (match (map-get? certifications id) c (ok (and (is-eq (get status c) "active") (< stacks-block-height (get valid-until c)))) (ok false)))
