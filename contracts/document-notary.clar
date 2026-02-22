;; Document Notary Contract
;; Notarize documents on-chain with hash verification
;; Halal - trust and verification
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-ALREADY-EXISTS (err u402))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var document-count uint u0)

(define-map documents (buff 32) {
  owner: principal, title: (string-utf8 100), notarized-at: uint, revoked: bool
})
(define-map document-index uint (buff 32))
(define-map owner-doc-count principal uint)
(define-map notaries principal bool)

(map-set notaries CONTRACT-OWNER true)

(define-public (add-notary (notary principal))
  (begin (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED) (map-set notaries notary true) (ok true)))

(define-public (notarize (doc-hash (buff 32)) (title (string-utf8 100)))
  (let ((id (+ (var-get document-count) u1)))
    (asserts! (is-none (map-get? documents doc-hash)) ERR-ALREADY-EXISTS)
    (map-set documents doc-hash { owner: tx-sender, title: title, notarized-at: stacks-block-height, revoked: false })
    (map-set document-index id doc-hash)
    (map-set owner-doc-count tx-sender (+ (default-to u0 (map-get? owner-doc-count tx-sender)) u1))
    (var-set document-count id)
    (print { event: "notarized", hash: doc-hash, owner: tx-sender })
    (ok id)))

(define-public (transfer-ownership (doc-hash (buff 32)) (new-owner principal))
  (let ((doc (unwrap! (map-get? documents doc-hash) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner doc)) ERR-NOT-AUTHORIZED)
    (map-set documents doc-hash (merge doc { owner: new-owner }))
    (ok true)))

(define-public (revoke-document (doc-hash (buff 32)))
  (let ((doc (unwrap! (map-get? documents doc-hash) ERR-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender (get owner doc)) (default-to false (map-get? notaries tx-sender))) ERR-NOT-AUTHORIZED)
    (map-set documents doc-hash (merge doc { revoked: true }))
    (ok true)))

(define-read-only (verify-document (doc-hash (buff 32))) (map-get? documents doc-hash))
(define-read-only (get-document-by-index (idx uint)) (map-get? document-index idx))
(define-read-only (get-document-count) (ok (var-get document-count)))
(define-read-only (get-owner-count (owner principal)) (ok (default-to u0 (map-get? owner-doc-count owner))))
(define-read-only (is-valid (doc-hash (buff 32)))
  (match (map-get? documents doc-hash) d (ok (not (get revoked d))) (ok false)))
