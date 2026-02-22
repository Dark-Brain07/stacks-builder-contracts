;; Achievement Badges Contract
;; Issue achievement NFTs for milestones
;; Halal - recognition and education
;; Clarity 4 compatible

(define-non-fungible-token badge uint)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-HAS (err u405))

(define-data-var badge-count uint u0)
(define-data-var template-count uint u0)

(define-map badge-templates uint { name: (string-utf8 50), description: (string-utf8 200), category: (string-ascii 20), max-supply: uint, minted: uint })
(define-map badge-info uint { template: uint, recipient: principal, earned-at: uint })
(define-map user-badges { user: principal, template: uint } bool)

(define-public (create-template (name (string-utf8 50)) (description (string-utf8 200)) (category (string-ascii 20)) (max-supply uint))
  (let ((id (+ (var-get template-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set badge-templates id { name: name, description: description, category: category, max-supply: max-supply, minted: u0 })
    (var-set template-count id) (ok id)))

(define-public (award-badge (template-id uint) (recipient principal))
  (let (
    (template (unwrap! (map-get? badge-templates template-id) ERR-NOT-FOUND))
    (id (+ (var-get badge-count) u1))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? user-badges { user: recipient, template: template-id })) ERR-ALREADY-HAS)
    (try! (nft-mint? badge id recipient))
    (map-set badge-info id { template: template-id, recipient: recipient, earned-at: stacks-block-height })
    (map-set user-badges { user: recipient, template: template-id } true)
    (map-set badge-templates template-id (merge template { minted: (+ (get minted template) u1) }))
    (var-set badge-count id) (ok id)))

(define-read-only (get-badge (id uint)) (map-get? badge-info id))
(define-read-only (get-template (id uint)) (map-get? badge-templates id))
(define-read-only (get-badge-count) (ok (var-get badge-count)))
(define-read-only (get-template-count) (ok (var-get template-count)))
(define-read-only (has-badge (user principal) (template uint)) (ok (default-to false (map-get? user-badges { user: user, template: template }))))
(define-read-only (get-badge-owner (id uint)) (nft-get-owner? badge id))
