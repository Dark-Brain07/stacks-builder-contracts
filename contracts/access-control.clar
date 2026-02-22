;; Access Control Contract
;; Role-based access management system
;; Halal - organizational governance
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-EXISTS (err u405))

(define-data-var role-count uint u0)

(define-map roles uint { name: (string-ascii 30), description: (string-utf8 100), level: uint, created: uint })
(define-map role-names (string-ascii 30) uint)
(define-map user-roles { user: principal, role-id: uint } { granted-by: principal, granted-at: uint })
(define-map user-role-count principal uint)
(define-map admins principal bool)

(map-set admins CONTRACT-OWNER true)

(define-public (add-admin (admin principal))
  (begin (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED) (map-set admins admin true) (ok true)))

(define-public (create-role (name (string-ascii 30)) (description (string-utf8 100)) (level uint))
  (let ((id (+ (var-get role-count) u1)))
    (asserts! (default-to false (map-get? admins tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? role-names name)) ERR-ALREADY-EXISTS)
    (map-set roles id { name: name, description: description, level: level, created: stacks-block-height })
    (map-set role-names name id)
    (var-set role-count id) (ok id)))

(define-public (grant-role (user principal) (role-id uint))
  (begin
    (asserts! (default-to false (map-get? admins tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? roles role-id)) ERR-NOT-FOUND)
    (map-set user-roles { user: user, role-id: role-id } { granted-by: tx-sender, granted-at: stacks-block-height })
    (map-set user-role-count user (+ (default-to u0 (map-get? user-role-count user)) u1))
    (ok true)))

(define-public (revoke-role (user principal) (role-id uint))
  (begin
    (asserts! (default-to false (map-get? admins tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? user-roles { user: user, role-id: role-id })) ERR-NOT-FOUND)
    (map-delete user-roles { user: user, role-id: role-id })
    (map-set user-role-count user (- (default-to u1 (map-get? user-role-count user)) u1))
    (ok true)))

(define-read-only (get-role (id uint)) (map-get? roles id))
(define-read-only (get-role-by-name (name (string-ascii 30))) (map-get? role-names name))
(define-read-only (has-role (user principal) (role-id uint)) (ok (is-some (map-get? user-roles { user: user, role-id: role-id }))))
(define-read-only (get-user-role-info (user principal) (role-id uint)) (map-get? user-roles { user: user, role-id: role-id }))
(define-read-only (get-role-count) (ok (var-get role-count)))
(define-read-only (get-user-roles-count (user principal)) (ok (default-to u0 (map-get? user-role-count user))))
