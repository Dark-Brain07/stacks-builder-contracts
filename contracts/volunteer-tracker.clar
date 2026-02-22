;; Volunteer Tracker Contract
;; Track volunteer hours and contributions
;; Halal - community service
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-NOT-ORG (err u405))

(define-data-var org-count uint u0)
(define-data-var total-hours uint u0)

(define-map organizations uint { name: (string-utf8 100), admin: principal, total-hours: uint, volunteers: uint, active: bool })
(define-map volunteer-profiles principal { name: (string-utf8 50), total-hours: uint, organizations: uint })
(define-map volunteer-logs { org-id: uint, volunteer: principal, index: uint } { hours: uint, activity: (string-utf8 100), verified: bool, block: uint })
(define-map vol-log-count { org-id: uint, volunteer: principal } uint)

(define-public (create-org (name (string-utf8 100)))
  (let ((id (+ (var-get org-count) u1)))
    (map-set organizations id { name: name, admin: tx-sender, total-hours: u0, volunteers: u0, active: true })
    (var-set org-count id) (ok id)))

(define-public (register-volunteer (name (string-utf8 50)))
  (begin (map-set volunteer-profiles tx-sender { name: name, total-hours: u0, organizations: u0 }) (ok true)))

(define-public (log-hours (org-id uint) (hours uint) (activity (string-utf8 100)))
  (let (
    (org (unwrap! (map-get? organizations org-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? vol-log-count { org-id: org-id, volunteer: tx-sender })))
  )
    (asserts! (get active org) ERR-NOT-FOUND)
    (map-set volunteer-logs { org-id: org-id, volunteer: tx-sender, index: idx } { hours: hours, activity: activity, verified: false, block: stacks-block-height })
    (map-set vol-log-count { org-id: org-id, volunteer: tx-sender } (+ idx u1))
    (ok idx)))

(define-public (verify-hours (org-id uint) (volunteer principal) (log-index uint))
  (let (
    (org (unwrap! (map-get? organizations org-id) ERR-NOT-FOUND))
    (log (unwrap! (map-get? volunteer-logs { org-id: org-id, volunteer: volunteer, index: log-index }) ERR-NOT-FOUND))
    (vol (default-to { name: u"", total-hours: u0, organizations: u0 } (map-get? volunteer-profiles volunteer)))
  )
    (asserts! (is-eq tx-sender (get admin org)) ERR-NOT-AUTHORIZED)
    (map-set volunteer-logs { org-id: org-id, volunteer: volunteer, index: log-index } (merge log { verified: true }))
    (map-set volunteer-profiles volunteer (merge vol { total-hours: (+ (get total-hours vol) (get hours log)) }))
    (map-set organizations org-id (merge org { total-hours: (+ (get total-hours org) (get hours log)) }))
    (var-set total-hours (+ (var-get total-hours) (get hours log)))
    (ok true)))

(define-read-only (get-org (id uint)) (map-get? organizations id))
(define-read-only (get-volunteer (who principal)) (map-get? volunteer-profiles who))
(define-read-only (get-log (org-id uint) (volunteer principal) (index uint)) (map-get? volunteer-logs { org-id: org-id, volunteer: volunteer, index: index }))
(define-read-only (get-org-count) (ok (var-get org-count)))
(define-read-only (get-total-hours) (ok (var-get total-hours)))
