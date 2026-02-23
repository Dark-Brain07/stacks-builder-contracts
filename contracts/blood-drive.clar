;; Blood Drive Contract
;; Blood drive campaign management
;; Halal - saving lives through donation
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var drive-count uint u0)
(define-data-var total-donors uint u0)
(define-data-var total-units uint u0)

(define-map drives uint { organizer: principal, name: (string-utf8 100), location: (string-utf8 100), target-units: uint, collected-units: uint, donors: uint, blood-bank: (string-utf8 100), status: (string-ascii 20), scheduled: uint })
(define-map drive-donors { drive-id: uint, donor: principal } { blood-type: (string-ascii 5), units: uint, donated: uint })
(define-map donor-records principal { total-donations: uint, total-units: uint, last-donation: uint })

(define-public (create-drive (name (string-utf8 100)) (location (string-utf8 100)) (target uint) (blood-bank (string-utf8 100)) (date-block uint))
  (let ((id (+ (var-get drive-count) u1)))
    (map-set drives id { organizer: tx-sender, name: name, location: location, target-units: target, collected-units: u0, donors: u0, blood-bank: blood-bank, status: "scheduled", scheduled: (+ stacks-block-height date-block) })
    (var-set drive-count id) (ok id)))

(define-public (register-donor (drive-id uint) (blood-type (string-ascii 5)))
  (let ((d (unwrap! (map-get? drives drive-id) ERR-NOT-FOUND)))
    (map-set drive-donors { drive-id: drive-id, donor: tx-sender } { blood-type: blood-type, units: u0, donated: u0 })
    (map-set drives drive-id (merge d { donors: (+ (get donors d) u1) }))
    (var-set total-donors (+ (var-get total-donors) u1)) (ok true)))

(define-public (record-donation (drive-id uint) (donor principal) (units uint))
  (let (
    (d (unwrap! (map-get? drives drive-id) ERR-NOT-FOUND))
    (dd (unwrap! (map-get? drive-donors { drive-id: drive-id, donor: donor }) ERR-NOT-FOUND))
    (dr (default-to { total-donations: u0, total-units: u0, last-donation: u0 } (map-get? donor-records donor)))
  )
    (asserts! (is-eq tx-sender (get organizer d)) ERR-NOT-AUTHORIZED)
    (map-set drive-donors { drive-id: drive-id, donor: donor } (merge dd { units: (+ (get units dd) units), donated: stacks-block-height }))
    (map-set donor-records donor { total-donations: (+ (get total-donations dr) u1), total-units: (+ (get total-units dr) units), last-donation: stacks-block-height })
    (map-set drives drive-id (merge d { collected-units: (+ (get collected-units d) units), status: "active" }))
    (var-set total-units (+ (var-get total-units) units)) (ok units)))

(define-public (complete-drive (drive-id uint))
  (let ((d (unwrap! (map-get? drives drive-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer d)) ERR-NOT-AUTHORIZED)
    (map-set drives drive-id (merge d { status: "completed" })) (ok true)))

(define-read-only (get-drive (id uint)) (map-get? drives id))
(define-read-only (get-drive-donor (drive-id uint) (who principal)) (map-get? drive-donors { drive-id: drive-id, donor: who }))
(define-read-only (get-donor-record (who principal)) (map-get? donor-records who))
(define-read-only (get-drive-count) (ok (var-get drive-count)))
(define-read-only (get-total-units) (ok (var-get total-units)))
