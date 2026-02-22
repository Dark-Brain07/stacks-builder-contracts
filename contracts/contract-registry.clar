;; Contract Registry
;; Registry of deployed smart contracts
;; Halal - transparency
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-REGISTERED (err u405))

(define-data-var entry-count uint u0)
(define-data-var verified-count uint u0)

(define-map registry uint {
  deployer: principal, name: (string-utf8 100), description: (string-utf8 200),
  contract-type: (string-ascii 30), verified: bool, audited: bool, registered: uint
})
(define-map name-index (string-utf8 100) uint)
(define-map deployer-contracts { deployer: principal, index: uint } uint)
(define-map deployer-count principal uint)
(define-map auditors principal bool)

(map-set auditors CONTRACT-OWNER true)

(define-public (add-auditor (a principal))
  (begin (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED) (map-set auditors a true) (ok true)))

(define-public (register-contract (name (string-utf8 100)) (description (string-utf8 200)) (contract-type (string-ascii 30)))
  (let (
    (id (+ (var-get entry-count) u1))
    (deployer-idx (default-to u0 (map-get? deployer-count tx-sender)))
  )
    (asserts! (is-none (map-get? name-index name)) ERR-ALREADY-REGISTERED)
    (map-set registry id { deployer: tx-sender, name: name, description: description, contract-type: contract-type, verified: false, audited: false, registered: stacks-block-height })
    (map-set name-index name id)
    (map-set deployer-contracts { deployer: tx-sender, index: deployer-idx } id)
    (map-set deployer-count tx-sender (+ deployer-idx u1))
    (var-set entry-count id) (ok id)))

(define-public (verify-contract (entry-id uint))
  (let ((entry (unwrap! (map-get? registry entry-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set registry entry-id (merge entry { verified: true }))
    (var-set verified-count (+ (var-get verified-count) u1)) (ok true)))

(define-public (mark-audited (entry-id uint))
  (let ((entry (unwrap! (map-get? registry entry-id) ERR-NOT-FOUND)))
    (asserts! (default-to false (map-get? auditors tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set registry entry-id (merge entry { audited: true })) (ok true)))

(define-read-only (get-entry (id uint)) (map-get? registry id))
(define-read-only (get-by-name (name (string-utf8 100))) (map-get? name-index name))
(define-read-only (get-entry-count) (ok (var-get entry-count)))
(define-read-only (get-verified-count) (ok (var-get verified-count)))
(define-read-only (get-deployer-total (who principal)) (ok (default-to u0 (map-get? deployer-count who))))
