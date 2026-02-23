;; Nikah Registry Contract
;; Islamic marriage registration on-chain
;; Halal - recording nikah (marriage)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-REGISTERED (err u405))

(define-data-var marriage-count uint u0)

(define-map marriages uint {
  spouse1: principal, spouse2: principal, mahr: uint, mahr-paid: bool,
  officiant: principal, witnesses: uint, status: (string-ascii 20), date-block: uint
})
(define-map marriage-by-person principal uint)
(define-map witnesses { marriage-id: uint, witness: principal } { attested: uint })
(define-map officiants principal { name: (string-utf8 100), verified: bool, ceremonies: uint })

(define-public (register-officiant (name (string-utf8 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set officiants tx-sender { name: name, verified: true, ceremonies: u0 }) (ok true)))

(define-public (add-officiant (officiant principal) (name (string-utf8 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set officiants officiant { name: name, verified: true, ceremonies: u0 }) (ok true)))

(define-public (register-marriage (spouse2 principal) (mahr uint))
  (let (
    (id (+ (var-get marriage-count) u1))
    (off (unwrap! (map-get? officiants tx-sender) ERR-NOT-AUTHORIZED))
  )
    (asserts! (get verified off) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? marriage-by-person spouse2)) ERR-ALREADY-REGISTERED)
    (map-set marriages id { spouse1: tx-sender, spouse2: spouse2, mahr: mahr, mahr-paid: false, officiant: tx-sender, witnesses: u0, status: "registered", date-block: stacks-block-height })
    (map-set officiants tx-sender (merge off { ceremonies: (+ (get ceremonies off) u1) }))
    (var-set marriage-count id) (ok id)))

(define-public (pay-mahr (marriage-id uint))
  (let ((m (unwrap! (map-get? marriages marriage-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get spouse1 m)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? (get mahr m) tx-sender (get spouse2 m)))
    (map-set marriages marriage-id (merge m { mahr-paid: true })) (ok true)))

(define-public (attest-witness (marriage-id uint))
  (let ((m (unwrap! (map-get? marriages marriage-id) ERR-NOT-FOUND)))
    (asserts! (is-none (map-get? witnesses { marriage-id: marriage-id, witness: tx-sender })) ERR-ALREADY-REGISTERED)
    (map-set witnesses { marriage-id: marriage-id, witness: tx-sender } { attested: stacks-block-height })
    (map-set marriages marriage-id (merge m { witnesses: (+ (get witnesses m) u1) }))
    (ok true)))

(define-public (complete-registration (marriage-id uint))
  (let ((m (unwrap! (map-get? marriages marriage-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get officiant m)) ERR-NOT-AUTHORIZED)
    (asserts! (>= (get witnesses m) u2) ERR-NOT-FOUND)
    (map-set marriages marriage-id (merge m { status: "completed" }))
    (map-set marriage-by-person (get spouse1 m) marriage-id)
    (map-set marriage-by-person (get spouse2 m) marriage-id)
    (ok true)))

(define-read-only (get-marriage (id uint)) (map-get? marriages id))
(define-read-only (get-marriage-by-person (who principal)) (map-get? marriage-by-person who))
(define-read-only (get-officiant (who principal)) (map-get? officiants who))
(define-read-only (get-marriage-count) (ok (var-get marriage-count)))
