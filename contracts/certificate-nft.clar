;; Certificate NFT Contract
;; Issue verifiable certificates/credentials on-chain
;; Halal - education and verification
;; Clarity 4 compatible

(define-non-fungible-token certificate uint)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-ISSUER (err u402))
(define-constant ERR-CERT-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-REVOKED (err u405))

(define-data-var cert-count uint u0)

(define-map issuers principal bool)
(define-map cert-data uint {
  recipient: principal,
  issuer: principal,
  title: (string-utf8 100),
  description: (string-utf8 200),
  issued-at: uint,
  revoked: bool
})

(map-set issuers CONTRACT-OWNER true)

(define-public (add-issuer (issuer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set issuers issuer true)
    (ok true)))

(define-public (issue-certificate (recipient principal) (title (string-utf8 100)) (description (string-utf8 200)))
  (let ((id (+ (var-get cert-count) u1)))
    (asserts! (default-to false (map-get? issuers tx-sender)) ERR-NOT-ISSUER)
    (try! (nft-mint? certificate id recipient))
    (map-set cert-data id {
      recipient: recipient, issuer: tx-sender, title: title,
      description: description, issued-at: stacks-block-height, revoked: false
    })
    (var-set cert-count id)
    (print { event: "cert-issued", id: id, recipient: recipient })
    (ok id)))

(define-public (revoke-certificate (cert-id uint))
  (let ((cert (unwrap! (map-get? cert-data cert-id) ERR-CERT-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get issuer cert)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get revoked cert)) ERR-ALREADY-REVOKED)
    (map-set cert-data cert-id (merge cert { revoked: true }))
    (ok true)))

(define-public (transfer-certificate (cert-id uint) (recipient principal))
  (let ((cert (unwrap! (map-get? cert-data cert-id) ERR-CERT-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get recipient cert)) ERR-NOT-AUTHORIZED)
    (try! (nft-transfer? certificate cert-id tx-sender recipient))
    (map-set cert-data cert-id (merge cert { recipient: recipient }))
    (ok true)))

(define-read-only (get-certificate (cert-id uint)) (map-get? cert-data cert-id))
(define-read-only (get-cert-count) (ok (var-get cert-count)))
(define-read-only (is-valid (cert-id uint))
  (match (map-get? cert-data cert-id) c (ok (not (get revoked c))) (ok false)))
(define-read-only (get-cert-owner (cert-id uint)) (nft-get-owner? certificate cert-id))
