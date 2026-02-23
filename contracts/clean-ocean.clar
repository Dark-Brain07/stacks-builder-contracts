;; Clean Ocean Contract
;; Ocean cleanup initiative coordination
;; Halal - preserving creation
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var cleanup-count uint u0)
(define-data-var total-kg-collected uint u0)
(define-data-var total-volunteers uint u0)

(define-map cleanups uint { organizer: principal, beach: (string-utf8 100), region: (string-utf8 50), volunteers: uint, kg-collected: uint, status: (string-ascii 20), scheduled: uint })
(define-map cleanup-volunteers { cleanup-id: uint, volunteer: principal } { kg-collected: uint, hours: uint, joined: uint })
(define-map waste-breakdown { cleanup-id: uint, waste-type: (string-ascii 20) } uint)

(define-public (schedule-cleanup (beach (string-utf8 100)) (region (string-utf8 50)) (date-block uint))
  (let ((id (+ (var-get cleanup-count) u1)))
    (map-set cleanups id { organizer: tx-sender, beach: beach, region: region, volunteers: u0, kg-collected: u0, status: "scheduled", scheduled: (+ stacks-block-height date-block) })
    (var-set cleanup-count id) (ok id)))

(define-public (volunteer-signup (cleanup-id uint))
  (let ((c (unwrap! (map-get? cleanups cleanup-id) ERR-NOT-FOUND)))
    (map-set cleanup-volunteers { cleanup-id: cleanup-id, volunteer: tx-sender } { kg-collected: u0, hours: u0, joined: stacks-block-height })
    (map-set cleanups cleanup-id (merge c { volunteers: (+ (get volunteers c) u1) }))
    (var-set total-volunteers (+ (var-get total-volunteers) u1)) (ok true)))

(define-public (log-collection (cleanup-id uint) (volunteer principal) (kg uint) (hours uint) (waste-type (string-ascii 20)))
  (let (
    (c (unwrap! (map-get? cleanups cleanup-id) ERR-NOT-FOUND))
    (v (unwrap! (map-get? cleanup-volunteers { cleanup-id: cleanup-id, volunteer: volunteer }) ERR-NOT-FOUND))
    (prev-waste (default-to u0 (map-get? waste-breakdown { cleanup-id: cleanup-id, waste-type: waste-type })))
  )
    (asserts! (is-eq tx-sender (get organizer c)) ERR-NOT-AUTHORIZED)
    (map-set cleanup-volunteers { cleanup-id: cleanup-id, volunteer: volunteer } (merge v { kg-collected: (+ (get kg-collected v) kg), hours: (+ (get hours v) hours) }))
    (map-set waste-breakdown { cleanup-id: cleanup-id, waste-type: waste-type } (+ prev-waste kg))
    (map-set cleanups cleanup-id (merge c { kg-collected: (+ (get kg-collected c) kg), status: "in-progress" }))
    (var-set total-kg-collected (+ (var-get total-kg-collected) kg)) (ok kg)))

(define-public (complete-cleanup (cleanup-id uint))
  (let ((c (unwrap! (map-get? cleanups cleanup-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer c)) ERR-NOT-AUTHORIZED)
    (map-set cleanups cleanup-id (merge c { status: "completed" })) (ok true)))

(define-read-only (get-cleanup (id uint)) (map-get? cleanups id))
(define-read-only (get-volunteer (cleanup-id uint) (who principal)) (map-get? cleanup-volunteers { cleanup-id: cleanup-id, volunteer: who }))
(define-read-only (get-waste (cleanup-id uint) (waste-type (string-ascii 20))) (ok (default-to u0 (map-get? waste-breakdown { cleanup-id: cleanup-id, waste-type: waste-type }))))
(define-read-only (get-cleanup-count) (ok (var-get cleanup-count)))
(define-read-only (get-total-collected) (ok (var-get total-kg-collected)))
