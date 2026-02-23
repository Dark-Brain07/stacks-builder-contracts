;; Umrah Planner Contract
;; Umrah trip planning and group savings
;; Halal - facilitating pilgrimage
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-FULL (err u405))

(define-data-var group-count uint u0)
(define-data-var total-saved uint u0)

(define-map umrah-groups uint {
  organizer: principal, title: (string-utf8 100), departure-block: uint,
  cost-per-person: uint, max-members: uint, current-members: uint,
  total-collected: uint, status: (string-ascii 20), created: uint
})
(define-map group-members { group-id: uint, member: principal } { paid: uint, confirmed: bool })
(define-map itinerary { group-id: uint, day: uint } { activity: (string-utf8 200), location: (string-utf8 100) })

(define-public (create-group (title (string-utf8 100)) (departure uint) (cost uint) (max-members uint))
  (let ((id (+ (var-get group-count) u1)))
    (map-set umrah-groups id { organizer: tx-sender, title: title, departure-block: (+ stacks-block-height departure), cost-per-person: cost, max-members: max-members, current-members: u0, total-collected: u0, status: "open", created: stacks-block-height })
    (var-set group-count id) (ok id)))

(define-public (join-group (group-id uint))
  (let ((group (unwrap! (map-get? umrah-groups group-id) ERR-NOT-FOUND)))
    (asserts! (is-eq (get status group) "open") ERR-NOT-FOUND)
    (asserts! (< (get current-members group) (get max-members group)) ERR-FULL)
    (try! (stx-transfer? (get cost-per-person group) tx-sender (get organizer group)))
    (map-set group-members { group-id: group-id, member: tx-sender } { paid: (get cost-per-person group), confirmed: true })
    (map-set umrah-groups group-id (merge group { current-members: (+ (get current-members group) u1), total-collected: (+ (get total-collected group) (get cost-per-person group)) }))
    (var-set total-saved (+ (var-get total-saved) (get cost-per-person group)))
    (ok true)))

(define-public (add-itinerary (group-id uint) (day uint) (activity (string-utf8 200)) (location (string-utf8 100)))
  (let ((group (unwrap! (map-get? umrah-groups group-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer group)) ERR-NOT-AUTHORIZED)
    (map-set itinerary { group-id: group-id, day: day } { activity: activity, location: location })
    (ok true)))

(define-public (close-group (group-id uint))
  (let ((group (unwrap! (map-get? umrah-groups group-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer group)) ERR-NOT-AUTHORIZED)
    (map-set umrah-groups group-id (merge group { status: "closed" })) (ok true)))

(define-public (complete-trip (group-id uint))
  (let ((group (unwrap! (map-get? umrah-groups group-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer group)) ERR-NOT-AUTHORIZED)
    (map-set umrah-groups group-id (merge group { status: "completed" })) (ok true)))

(define-read-only (get-group (id uint)) (map-get? umrah-groups id))
(define-read-only (get-member (group-id uint) (member principal)) (map-get? group-members { group-id: group-id, member: member }))
(define-read-only (get-itinerary-day (group-id uint) (day uint)) (map-get? itinerary { group-id: group-id, day: day }))
(define-read-only (get-group-count) (ok (var-get group-count)))
(define-read-only (get-total-saved) (ok (var-get total-saved)))
