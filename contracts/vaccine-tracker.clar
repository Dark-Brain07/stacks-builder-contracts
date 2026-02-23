;; Vaccine Tracker Contract
;; Vaccination record tracking
;; Halal - public health protection
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var patient-count uint u0)
(define-data-var vaccine-record-count uint u0)

(define-map patients uint { registered-by: principal, age: uint, records: uint, registered: uint })
(define-map vaccine-records uint { patient-id: uint, vaccine-name: (string-utf8 100), dose-number: uint, provider: principal, batch-number: (string-ascii 30), administered: uint, next-due: uint })
(define-map authorized-providers principal { facility: (string-utf8 100), active: bool })

(define-public (authorize-provider (provider principal) (facility (string-utf8 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set authorized-providers provider { facility: facility, active: true }) (ok true)))

(define-public (register-patient (age uint))
  (let ((id (+ (var-get patient-count) u1)))
    (asserts! (is-some (map-get? authorized-providers tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set patients id { registered-by: tx-sender, age: age, records: u0, registered: stacks-block-height })
    (var-set patient-count id) (ok id)))

(define-public (record-vaccine (patient-id uint) (vaccine-name (string-utf8 100)) (dose uint) (batch (string-ascii 30)) (next-due-blocks uint))
  (let (
    (p (unwrap! (map-get? patients patient-id) ERR-NOT-FOUND))
    (prov (unwrap! (map-get? authorized-providers tx-sender) ERR-NOT-AUTHORIZED))
    (rid (+ (var-get vaccine-record-count) u1))
  )
    (asserts! (get active prov) ERR-NOT-AUTHORIZED)
    (map-set vaccine-records rid { patient-id: patient-id, vaccine-name: vaccine-name, dose-number: dose, provider: tx-sender, batch-number: batch, administered: stacks-block-height, next-due: (+ stacks-block-height next-due-blocks) })
    (map-set patients patient-id (merge p { records: (+ (get records p) u1) }))
    (var-set vaccine-record-count rid) (ok rid)))

(define-public (revoke-provider (provider principal))
  (let ((prov (unwrap! (map-get? authorized-providers provider) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set authorized-providers provider (merge prov { active: false })) (ok true)))

(define-read-only (get-patient (id uint)) (map-get? patients id))
(define-read-only (get-record (id uint)) (map-get? vaccine-records id))
(define-read-only (get-provider (who principal)) (map-get? authorized-providers who))
(define-read-only (get-patient-count) (ok (var-get patient-count)))
(define-read-only (get-record-count) (ok (var-get vaccine-record-count)))
