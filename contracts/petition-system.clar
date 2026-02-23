;; Petition System Contract
;; Public petition creation and signing
;; Halal - civic engagement
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-SIGNED (err u405))
(define-constant ERR-CLOSED (err u406))

(define-data-var petition-count uint u0)
(define-data-var total-signatures uint u0)

(define-map petitions uint {
  creator: principal, title: (string-utf8 100), description: (string-utf8 200),
  target-signatures: uint, current-signatures: uint, status: (string-ascii 20), created: uint
})
(define-map signatures { petition-id: uint, signer: principal } { signed-at: uint })
(define-map petition-signers { petition-id: uint, index: uint } principal)

(define-public (create-petition (title (string-utf8 100)) (description (string-utf8 200)) (target uint))
  (let ((id (+ (var-get petition-count) u1)))
    (map-set petitions id {
      creator: tx-sender, title: title, description: description,
      target-signatures: target, current-signatures: u0, status: "open", created: stacks-block-height
    })
    (var-set petition-count id) (ok id)))

(define-public (sign-petition (petition-id uint))
  (let ((petition (unwrap! (map-get? petitions petition-id) ERR-NOT-FOUND)))
    (asserts! (is-eq (get status petition) "open") ERR-CLOSED)
    (asserts! (is-none (map-get? signatures { petition-id: petition-id, signer: tx-sender })) ERR-ALREADY-SIGNED)
    (map-set signatures { petition-id: petition-id, signer: tx-sender } { signed-at: stacks-block-height })
    (map-set petition-signers { petition-id: petition-id, index: (get current-signatures petition) } tx-sender)
    (let ((new-count (+ (get current-signatures petition) u1)))
      (map-set petitions petition-id (merge petition {
        current-signatures: new-count,
        status: (if (>= new-count (get target-signatures petition)) "target-met" "open")
      }))
      (var-set total-signatures (+ (var-get total-signatures) u1))
      (ok new-count))))

(define-public (close-petition (petition-id uint))
  (let ((petition (unwrap! (map-get? petitions petition-id) ERR-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender (get creator petition)) (is-eq tx-sender CONTRACT-OWNER)) ERR-NOT-AUTHORIZED)
    (map-set petitions petition-id (merge petition { status: "closed" })) (ok true)))

(define-read-only (get-petition (id uint)) (map-get? petitions id))
(define-read-only (has-signed (petition-id uint) (signer principal)) (ok (is-some (map-get? signatures { petition-id: petition-id, signer: signer }))))
(define-read-only (get-signer-at (petition-id uint) (index uint)) (map-get? petition-signers { petition-id: petition-id, index: index }))
(define-read-only (get-petition-count) (ok (var-get petition-count)))
(define-read-only (get-total-signatures) (ok (var-get total-signatures)))
