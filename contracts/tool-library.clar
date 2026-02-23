;; Tool Library Contract
;; Community tool sharing library
;; Halal - cooperative sharing
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-UNAVAILABLE (err u405))

(define-data-var tool-count uint u0)
(define-data-var borrow-count uint u0)

(define-map tools uint { owner: principal, name: (string-utf8 100), category: (string-ascii 20), condition: (string-ascii 20), available: bool, times-borrowed: uint })
(define-map borrows uint { tool-id: uint, borrower: principal, borrowed: uint, due: uint, returned: bool })
(define-map member-stats principal { tools-donated: uint, tools-borrowed: uint })

(define-public (donate-tool (name (string-utf8 100)) (category (string-ascii 20)) (condition (string-ascii 20)))
  (let (
    (id (+ (var-get tool-count) u1))
    (stats (default-to { tools-donated: u0, tools-borrowed: u0 } (map-get? member-stats tx-sender)))
  )
    (map-set tools id { owner: tx-sender, name: name, category: category, condition: condition, available: true, times-borrowed: u0 })
    (map-set member-stats tx-sender (merge stats { tools-donated: (+ (get tools-donated stats) u1) }))
    (var-set tool-count id) (ok id)))

(define-public (borrow-tool (tool-id uint) (duration uint))
  (let (
    (tool (unwrap! (map-get? tools tool-id) ERR-NOT-FOUND))
    (bid (+ (var-get borrow-count) u1))
    (stats (default-to { tools-donated: u0, tools-borrowed: u0 } (map-get? member-stats tx-sender)))
  )
    (asserts! (get available tool) ERR-UNAVAILABLE)
    (map-set borrows bid { tool-id: tool-id, borrower: tx-sender, borrowed: stacks-block-height, due: (+ stacks-block-height duration), returned: false })
    (map-set tools tool-id (merge tool { available: false, times-borrowed: (+ (get times-borrowed tool) u1) }))
    (map-set member-stats tx-sender (merge stats { tools-borrowed: (+ (get tools-borrowed stats) u1) }))
    (var-set borrow-count bid) (ok bid)))

(define-public (return-tool (borrow-id uint) (condition (string-ascii 20)))
  (let (
    (b (unwrap! (map-get? borrows borrow-id) ERR-NOT-FOUND))
    (tool (unwrap! (map-get? tools (get tool-id b)) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get borrower b)) ERR-NOT-AUTHORIZED)
    (map-set borrows borrow-id (merge b { returned: true }))
    (map-set tools (get tool-id b) (merge tool { available: true, condition: condition }))
    (ok true)))

(define-read-only (get-tool (id uint)) (map-get? tools id))
(define-read-only (get-borrow (id uint)) (map-get? borrows id))
(define-read-only (get-member (who principal)) (map-get? member-stats who))
(define-read-only (get-tool-count) (ok (var-get tool-count)))
(define-read-only (get-borrow-count) (ok (var-get borrow-count)))
