;; Dawah Center Contract
;; Islamic outreach center management
;; Halal - dawah (calling to good)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var event-count uint u0)
(define-data-var volunteer-count uint u0)
(define-data-var total-funded uint u0)

(define-map outreach-events uint { coordinator: principal, title: (string-utf8 100), location: (string-utf8 100), topic: (string-utf8 100), attendees: uint, volunteers: uint, budget: uint, status: (string-ascii 20), scheduled: uint })
(define-map volunteers principal { name: (string-utf8 100), skills: (string-utf8 200), events-served: uint, hours: uint, active: bool })
(define-map event-volunteers { event-id: uint, volunteer: principal } { role: (string-utf8 50), hours: uint })

(define-public (register-volunteer (name (string-utf8 100)) (skills (string-utf8 200)))
  (begin
    (map-set volunteers tx-sender { name: name, skills: skills, events-served: u0, hours: u0, active: true })
    (var-set volunteer-count (+ (var-get volunteer-count) u1)) (ok true)))

(define-public (create-event (title (string-utf8 100)) (location (string-utf8 100)) (topic (string-utf8 100)) (budget uint) (scheduled uint))
  (let ((id (+ (var-get event-count) u1)))
    (if (> budget u0) (try! (stx-transfer? budget tx-sender CONTRACT-OWNER)) true)
    (map-set outreach-events id { coordinator: tx-sender, title: title, location: location, topic: topic, attendees: u0, volunteers: u0, budget: budget, status: "planned", scheduled: (+ stacks-block-height scheduled) })
    (var-set event-count id)
    (var-set total-funded (+ (var-get total-funded) budget)) (ok id)))

(define-public (sign-up-event (event-id uint) (role (string-utf8 50)))
  (let (
    (ev (unwrap! (map-get? outreach-events event-id) ERR-NOT-FOUND))
    (v (unwrap! (map-get? volunteers tx-sender) ERR-NOT-FOUND))
  )
    (map-set event-volunteers { event-id: event-id, volunteer: tx-sender } { role: role, hours: u0 })
    (map-set outreach-events event-id (merge ev { volunteers: (+ (get volunteers ev) u1) })) (ok true)))

(define-public (log-attendance (event-id uint) (attendees uint))
  (let ((ev (unwrap! (map-get? outreach-events event-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get coordinator ev)) ERR-NOT-AUTHORIZED)
    (map-set outreach-events event-id (merge ev { attendees: attendees, status: "completed" })) (ok attendees)))

(define-public (log-volunteer-hours (event-id uint) (volunteer principal) (hours uint))
  (let (
    (ev (unwrap! (map-get? outreach-events event-id) ERR-NOT-FOUND))
    (evol (unwrap! (map-get? event-volunteers { event-id: event-id, volunteer: volunteer }) ERR-NOT-FOUND))
    (v (unwrap! (map-get? volunteers volunteer) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get coordinator ev)) ERR-NOT-AUTHORIZED)
    (map-set event-volunteers { event-id: event-id, volunteer: volunteer } (merge evol { hours: hours }))
    (map-set volunteers volunteer (merge v { events-served: (+ (get events-served v) u1), hours: (+ (get hours v) hours) })) (ok hours)))

(define-read-only (get-event (id uint)) (map-get? outreach-events id))
(define-read-only (get-volunteer (who principal)) (map-get? volunteers who))
(define-read-only (get-event-volunteer (event-id uint) (who principal)) (map-get? event-volunteers { event-id: event-id, volunteer: who }))
(define-read-only (get-event-count) (ok (var-get event-count)))
(define-read-only (get-volunteer-count) (ok (var-get volunteer-count)))
