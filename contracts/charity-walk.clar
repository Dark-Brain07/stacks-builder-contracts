;; Charity Walk Contract
;; Charity walkathon fundraising events
;; Halal - physical charity effort
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var event-count uint u0)
(define-data-var total-raised uint u0)

(define-map events uint {
  organizer: principal, name: (string-utf8 100), beneficiary: (string-utf8 100),
  distance-km: uint, walkers: uint, goal: uint, raised: uint,
  event-block: uint, status: (string-ascii 20)
})
(define-map walker-records { event-id: uint, walker: principal } { pledges: uint, sponsors: uint, completed: bool })
(define-map sponsorships { event-id: uint, walker: principal, sponsor: principal } { amount: uint, paid: bool })

(define-public (create-event (name (string-utf8 100)) (beneficiary (string-utf8 100)) (distance uint) (goal uint) (event-block uint))
  (let ((id (+ (var-get event-count) u1)))
    (map-set events id { organizer: tx-sender, name: name, beneficiary: beneficiary, distance-km: distance, walkers: u0, goal: goal, raised: u0, event-block: event-block, status: "open" })
    (var-set event-count id) (ok id)))

(define-public (register-walker (event-id uint))
  (let ((ev (unwrap! (map-get? events event-id) ERR-NOT-FOUND)))
    (map-set walker-records { event-id: event-id, walker: tx-sender } { pledges: u0, sponsors: u0, completed: false })
    (map-set events event-id (merge ev { walkers: (+ (get walkers ev) u1) })) (ok true)))

(define-public (sponsor-walker (event-id uint) (walker principal) (amount uint))
  (let (
    (ev (unwrap! (map-get? events event-id) ERR-NOT-FOUND))
    (wr (unwrap! (map-get? walker-records { event-id: event-id, walker: walker }) ERR-NOT-FOUND))
  )
    (try! (stx-transfer? amount tx-sender (get organizer ev)))
    (map-set sponsorships { event-id: event-id, walker: walker, sponsor: tx-sender } { amount: amount, paid: true })
    (map-set walker-records { event-id: event-id, walker: walker } (merge wr { pledges: (+ (get pledges wr) amount), sponsors: (+ (get sponsors wr) u1) }))
    (map-set events event-id (merge ev { raised: (+ (get raised ev) amount) }))
    (var-set total-raised (+ (var-get total-raised) amount)) (ok amount)))

(define-public (complete-walk (event-id uint) (walker principal))
  (let ((wr (unwrap! (map-get? walker-records { event-id: event-id, walker: walker }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set walker-records { event-id: event-id, walker: walker } (merge wr { completed: true })) (ok true)))

(define-public (close-event (event-id uint))
  (let ((ev (unwrap! (map-get? events event-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer ev)) ERR-NOT-AUTHORIZED)
    (map-set events event-id (merge ev { status: "closed" })) (ok true)))

(define-read-only (get-event (id uint)) (map-get? events id))
(define-read-only (get-walker (event-id uint) (walker principal)) (map-get? walker-records { event-id: event-id, walker: walker }))
(define-read-only (get-sponsorship (event-id uint) (walker principal) (sponsor principal)) (map-get? sponsorships { event-id: event-id, walker: walker, sponsor: sponsor }))
(define-read-only (get-event-count) (ok (var-get event-count)))
(define-read-only (get-total-raised) (ok (var-get total-raised)))
