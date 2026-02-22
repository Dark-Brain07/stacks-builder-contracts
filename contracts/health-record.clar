;; Health Record Contract
;; Secure on-chain medical record management
;; Halal - healthcare and wellbeing
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-GRANTED (err u405))

(define-data-var record-count uint u0)

(define-map patients principal { registered: uint, record-count: uint })
(define-map records { patient: principal, index: uint } { provider: principal, record-type: (string-ascii 30), summary: (string-utf8 200), block: uint })
(define-map access-grants { patient: principal, provider: principal } { granted-at: uint, expires-at: uint })
(define-map providers principal { name: (string-utf8 100), verified: bool })

(define-public (register-patient)
  (begin
    (map-set patients tx-sender { registered: stacks-block-height, record-count: u0 })
    (ok true)))

(define-public (register-provider (name (string-utf8 100)))
  (begin
    (map-set providers tx-sender { name: name, verified: false })
    (ok true)))

(define-public (verify-provider (provider principal))
  (let ((prov (unwrap! (map-get? providers provider) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set providers provider (merge prov { verified: true })) (ok true)))

(define-public (grant-access (provider principal) (duration uint))
  (begin
    (asserts! (is-some (map-get? patients tx-sender)) ERR-NOT-FOUND)
    (map-set access-grants { patient: tx-sender, provider: provider } { granted-at: stacks-block-height, expires-at: (+ stacks-block-height duration) })
    (ok true)))

(define-public (revoke-access (provider principal))
  (begin
    (map-delete access-grants { patient: tx-sender, provider: provider })
    (ok true)))

(define-public (add-record (patient principal) (record-type (string-ascii 30)) (summary (string-utf8 200)))
  (let (
    (pat (unwrap! (map-get? patients patient) ERR-NOT-FOUND))
    (grant (unwrap! (map-get? access-grants { patient: patient, provider: tx-sender }) ERR-NOT-AUTHORIZED))
    (idx (get record-count pat))
  )
    (asserts! (>= (get expires-at grant) stacks-block-height) ERR-NOT-AUTHORIZED)
    (map-set records { patient: patient, index: idx } { provider: tx-sender, record-type: record-type, summary: summary, block: stacks-block-height })
    (map-set patients patient (merge pat { record-count: (+ idx u1) }))
    (var-set record-count (+ (var-get record-count) u1))
    (ok idx)))

(define-read-only (get-patient (who principal)) (map-get? patients who))
(define-read-only (get-record (patient principal) (index uint)) (map-get? records { patient: patient, index: index }))
(define-read-only (get-provider (who principal)) (map-get? providers who))
(define-read-only (has-access (patient principal) (provider principal))
  (match (map-get? access-grants { patient: patient, provider: provider }) g (ok (>= (get expires-at g) stacks-block-height)) (ok false)))
(define-read-only (get-total-records) (ok (var-get record-count)))
