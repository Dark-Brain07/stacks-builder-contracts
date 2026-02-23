;; Charity Run Contract
;; Charity marathon fundraising
;; Halal - physical effort for charity
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var race-count uint u0)
(define-data-var total-raised uint u0)

(define-map races uint { organizer: principal, name: (string-utf8 100), cause: (string-utf8 100), distance-km: uint, entry-fee: uint, runners: uint, raised: uint, beneficiary: principal, status: (string-ascii 20) })
(define-map runners { race-id: uint, runner: principal } { bib: uint, finish-time: uint, finished: bool, pledges: uint })
(define-map runner-donations { race-id: uint, runner: principal, donor: principal } uint)

(define-public (create-race (name (string-utf8 100)) (cause (string-utf8 100)) (distance uint) (fee uint) (beneficiary principal))
  (let ((id (+ (var-get race-count) u1)))
    (map-set races id { organizer: tx-sender, name: name, cause: cause, distance-km: distance, entry-fee: fee, runners: u0, raised: u0, beneficiary: beneficiary, status: "open" })
    (var-set race-count id) (ok id)))

(define-public (register-runner (race-id uint))
  (let (
    (race (unwrap! (map-get? races race-id) ERR-NOT-FOUND))
    (bib (+ (get runners race) u1))
  )
    (try! (stx-transfer? (get entry-fee race) tx-sender (get organizer race)))
    (map-set runners { race-id: race-id, runner: tx-sender } { bib: bib, finish-time: u0, finished: false, pledges: u0 })
    (map-set races race-id (merge race { runners: bib, raised: (+ (get raised race) (get entry-fee race)) }))
    (var-set total-raised (+ (var-get total-raised) (get entry-fee race))) (ok bib)))

(define-public (pledge-runner (race-id uint) (runner principal) (amount uint))
  (let (
    (race (unwrap! (map-get? races race-id) ERR-NOT-FOUND))
    (r (unwrap! (map-get? runners { race-id: race-id, runner: runner }) ERR-NOT-FOUND))
  )
    (try! (stx-transfer? amount tx-sender (get beneficiary race)))
    (map-set runner-donations { race-id: race-id, runner: runner, donor: tx-sender } amount)
    (map-set runners { race-id: race-id, runner: runner } (merge r { pledges: (+ (get pledges r) amount) }))
    (map-set races race-id (merge race { raised: (+ (get raised race) amount) }))
    (var-set total-raised (+ (var-get total-raised) amount)) (ok amount)))

(define-public (finish-runner (race-id uint) (runner principal) (time uint))
  (let ((r (unwrap! (map-get? runners { race-id: race-id, runner: runner }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set runners { race-id: race-id, runner: runner } (merge r { finish-time: time, finished: true })) (ok true)))

(define-public (close-race (race-id uint))
  (let ((race (unwrap! (map-get? races race-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer race)) ERR-NOT-AUTHORIZED)
    (map-set races race-id (merge race { status: "closed" })) (ok true)))

(define-read-only (get-race (id uint)) (map-get? races id))
(define-read-only (get-runner (race-id uint) (who principal)) (map-get? runners { race-id: race-id, runner: who }))
(define-read-only (get-race-count) (ok (var-get race-count)))
(define-read-only (get-total-raised) (ok (var-get total-raised)))
