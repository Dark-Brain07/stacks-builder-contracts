;; Clothing Drive Contract
;; Clothing donation and distribution
;; Halal - clothing the needy
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var drive-count uint u0)
(define-data-var total-items uint u0)

(define-map drives uint { organizer: principal, name: (string-utf8 100), location: (string-utf8 100), items-collected: uint, items-distributed: uint, status: (string-ascii 20), created: uint })
(define-map donations { drive-id: uint, donor: principal } { items: uint, category: (string-ascii 20), block: uint })
(define-map distributions { drive-id: uint, index: uint } { items: uint, recipient-group: (string-utf8 100), block: uint })
(define-map dist-count uint uint)

(define-public (create-drive (name (string-utf8 100)) (location (string-utf8 100)))
  (let ((id (+ (var-get drive-count) u1)))
    (map-set drives id { organizer: tx-sender, name: name, location: location, items-collected: u0, items-distributed: u0, status: "active", created: stacks-block-height })
    (var-set drive-count id) (ok id)))

(define-public (donate-clothes (drive-id uint) (items uint) (category (string-ascii 20)))
  (let ((d (unwrap! (map-get? drives drive-id) ERR-NOT-FOUND)))
    (map-set donations { drive-id: drive-id, donor: tx-sender } { items: items, category: category, block: stacks-block-height })
    (map-set drives drive-id (merge d { items-collected: (+ (get items-collected d) items) }))
    (var-set total-items (+ (var-get total-items) items)) (ok items)))

(define-public (distribute (drive-id uint) (items uint) (recipient-group (string-utf8 100)))
  (let (
    (d (unwrap! (map-get? drives drive-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? dist-count drive-id)))
  )
    (asserts! (is-eq tx-sender (get organizer d)) ERR-NOT-AUTHORIZED)
    (map-set distributions { drive-id: drive-id, index: idx } { items: items, recipient-group: recipient-group, block: stacks-block-height })
    (map-set dist-count drive-id (+ idx u1))
    (map-set drives drive-id (merge d { items-distributed: (+ (get items-distributed d) items) })) (ok items)))

(define-public (close-drive (drive-id uint))
  (let ((d (unwrap! (map-get? drives drive-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer d)) ERR-NOT-AUTHORIZED)
    (map-set drives drive-id (merge d { status: "closed" })) (ok true)))

(define-read-only (get-drive (id uint)) (map-get? drives id))
(define-read-only (get-donation (drive-id uint) (donor principal)) (map-get? donations { drive-id: drive-id, donor: donor }))
(define-read-only (get-distribution (drive-id uint) (index uint)) (map-get? distributions { drive-id: drive-id, index: index }))
(define-read-only (get-drive-count) (ok (var-get drive-count)))
(define-read-only (get-total-items) (ok (var-get total-items)))
