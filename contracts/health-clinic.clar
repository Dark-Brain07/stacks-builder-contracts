;; Health Clinic Contract
;; Free community health clinic management
;; Halal - healing the sick
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var patient-count uint u0)
(define-data-var visit-count uint u0)
(define-data-var total-donations uint u0)

(define-map doctors principal { name: (string-utf8 100), specialty: (string-utf8 50), patients-seen: uint, active: bool })
(define-map patients uint { registered-by: principal, age: uint, condition: (string-utf8 200), visits: uint, registered: uint })
(define-map clinic-visits uint { patient-id: uint, doctor: principal, diagnosis: (string-utf8 200), treatment: (string-utf8 200), block: uint })
(define-map clinic-donations principal uint)

(define-public (register-doctor (name (string-utf8 100)) (specialty (string-utf8 50)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set doctors tx-sender { name: name, specialty: specialty, patients-seen: u0, active: true }) (ok true)))

(define-public (add-doctor (doc principal) (name (string-utf8 100)) (specialty (string-utf8 50)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set doctors doc { name: name, specialty: specialty, patients-seen: u0, active: true }) (ok true)))

(define-public (register-patient (age uint) (condition (string-utf8 200)))
  (let ((id (+ (var-get patient-count) u1)))
    (map-set patients id { registered-by: tx-sender, age: age, condition: condition, visits: u0, registered: stacks-block-height })
    (var-set patient-count id) (ok id)))

(define-public (record-visit (patient-id uint) (diagnosis (string-utf8 200)) (treatment (string-utf8 200)))
  (let (
    (doc (unwrap! (map-get? doctors tx-sender) ERR-NOT-AUTHORIZED))
    (p (unwrap! (map-get? patients patient-id) ERR-NOT-FOUND))
    (vid (+ (var-get visit-count) u1))
  )
    (map-set clinic-visits vid { patient-id: patient-id, doctor: tx-sender, diagnosis: diagnosis, treatment: treatment, block: stacks-block-height })
    (map-set patients patient-id (merge p { visits: (+ (get visits p) u1) }))
    (map-set doctors tx-sender (merge doc { patients-seen: (+ (get patients-seen doc) u1) }))
    (var-set visit-count vid) (ok vid)))

(define-public (donate-clinic (amount uint))
  (let ((prev (default-to u0 (map-get? clinic-donations tx-sender))))
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set clinic-donations tx-sender (+ prev amount))
    (var-set total-donations (+ (var-get total-donations) amount)) (ok amount)))

(define-read-only (get-doctor (who principal)) (map-get? doctors who))
(define-read-only (get-patient (id uint)) (map-get? patients id))
(define-read-only (get-visit (id uint)) (map-get? clinic-visits id))
(define-read-only (get-patient-count) (ok (var-get patient-count)))
(define-read-only (get-visit-count) (ok (var-get visit-count)))
(define-read-only (get-total-donations) (ok (var-get total-donations)))
