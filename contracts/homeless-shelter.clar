;; Homeless Shelter Contract
;; Homeless shelter bed management and services
;; Halal - sheltering the homeless
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-NO-BEDS (err u405))

(define-data-var shelter-count uint u0)
(define-data-var total-sheltered uint u0)
(define-data-var total-donations uint u0)

(define-map shelters uint { manager: principal, name: (string-utf8 100), location: (string-utf8 100), total-beds: uint, occupied: uint, active: bool })
(define-map residents { shelter-id: uint, index: uint } { checked-in: uint, checked-out: uint, nights: uint, active: bool })
(define-map resident-count uint uint)
(define-map shelter-donations { shelter-id: uint, donor: principal } uint)

(define-public (register-shelter (name (string-utf8 100)) (location (string-utf8 100)) (beds uint))
  (let ((id (+ (var-get shelter-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set shelters id { manager: tx-sender, name: name, location: location, total-beds: beds, occupied: u0, active: true })
    (var-set shelter-count id) (ok id)))

(define-public (check-in (shelter-id uint))
  (let (
    (shelter (unwrap! (map-get? shelters shelter-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? resident-count shelter-id)))
  )
    (asserts! (is-eq tx-sender (get manager shelter)) ERR-NOT-AUTHORIZED)
    (asserts! (< (get occupied shelter) (get total-beds shelter)) ERR-NO-BEDS)
    (map-set residents { shelter-id: shelter-id, index: idx } { checked-in: stacks-block-height, checked-out: u0, nights: u0, active: true })
    (map-set resident-count shelter-id (+ idx u1))
    (map-set shelters shelter-id (merge shelter { occupied: (+ (get occupied shelter) u1) }))
    (var-set total-sheltered (+ (var-get total-sheltered) u1)) (ok idx)))

(define-public (check-out (shelter-id uint) (resident-index uint) (nights uint))
  (let (
    (shelter (unwrap! (map-get? shelters shelter-id) ERR-NOT-FOUND))
    (r (unwrap! (map-get? residents { shelter-id: shelter-id, index: resident-index }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get manager shelter)) ERR-NOT-AUTHORIZED)
    (map-set residents { shelter-id: shelter-id, index: resident-index } (merge r { checked-out: stacks-block-height, nights: nights, active: false }))
    (map-set shelters shelter-id (merge shelter { occupied: (- (get occupied shelter) u1) })) (ok true)))

(define-public (donate-shelter (shelter-id uint) (amount uint))
  (let (
    (shelter (unwrap! (map-get? shelters shelter-id) ERR-NOT-FOUND))
    (prev (default-to u0 (map-get? shelter-donations { shelter-id: shelter-id, donor: tx-sender })))
  )
    (try! (stx-transfer? amount tx-sender (get manager shelter)))
    (map-set shelter-donations { shelter-id: shelter-id, donor: tx-sender } (+ prev amount))
    (var-set total-donations (+ (var-get total-donations) amount)) (ok amount)))

(define-read-only (get-shelter (id uint)) (map-get? shelters id))
(define-read-only (get-resident (shelter-id uint) (index uint)) (map-get? residents { shelter-id: shelter-id, index: index }))
(define-read-only (get-shelter-count) (ok (var-get shelter-count)))
(define-read-only (get-total-sheltered) (ok (var-get total-sheltered)))
(define-read-only (get-total-donations) (ok (var-get total-donations)))
