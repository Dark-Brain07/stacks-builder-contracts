;; Youth Camp Contract
;; Youth camp enrollment and management
;; Halal - nurturing youth
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-FULL (err u405))

(define-data-var camp-count uint u0)
(define-data-var total-enrolled uint u0)

(define-map camps uint { organizer: principal, name: (string-utf8 100), theme: (string-utf8 100), capacity: uint, enrolled: uint, fee: uint, start-block: uint, duration: uint, status: (string-ascii 20) })
(define-map campers { camp-id: uint, camper: principal } { guardian: principal, age: uint, fee-paid: uint, enrolled-at: uint })
(define-map camp-activities { camp-id: uint, day: uint } { activity: (string-utf8 200), leader: (string-utf8 100) })

(define-public (create-camp (name (string-utf8 100)) (theme (string-utf8 100)) (capacity uint) (fee uint) (start uint) (duration uint))
  (let ((id (+ (var-get camp-count) u1)))
    (map-set camps id { organizer: tx-sender, name: name, theme: theme, capacity: capacity, enrolled: u0, fee: fee, start-block: (+ stacks-block-height start), duration: duration, status: "open" })
    (var-set camp-count id) (ok id)))

(define-public (enroll-camper (camp-id uint) (camper principal) (age uint))
  (let ((camp (unwrap! (map-get? camps camp-id) ERR-NOT-FOUND)))
    (asserts! (< (get enrolled camp) (get capacity camp)) ERR-FULL)
    (try! (stx-transfer? (get fee camp) tx-sender (get organizer camp)))
    (map-set campers { camp-id: camp-id, camper: camper } { guardian: tx-sender, age: age, fee-paid: (get fee camp), enrolled-at: stacks-block-height })
    (map-set camps camp-id (merge camp { enrolled: (+ (get enrolled camp) u1) }))
    (var-set total-enrolled (+ (var-get total-enrolled) u1)) (ok true)))

(define-public (add-activity (camp-id uint) (day uint) (activity (string-utf8 200)) (leader (string-utf8 100)))
  (let ((camp (unwrap! (map-get? camps camp-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer camp)) ERR-NOT-AUTHORIZED)
    (map-set camp-activities { camp-id: camp-id, day: day } { activity: activity, leader: leader }) (ok true)))

(define-public (close-camp (camp-id uint))
  (let ((camp (unwrap! (map-get? camps camp-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer camp)) ERR-NOT-AUTHORIZED)
    (map-set camps camp-id (merge camp { status: "completed" })) (ok true)))

(define-read-only (get-camp (id uint)) (map-get? camps id))
(define-read-only (get-camper (camp-id uint) (who principal)) (map-get? campers { camp-id: camp-id, camper: who }))
(define-read-only (get-activity (camp-id uint) (day uint)) (map-get? camp-activities { camp-id: camp-id, day: day }))
(define-read-only (get-camp-count) (ok (var-get camp-count)))
(define-read-only (get-total-enrolled) (ok (var-get total-enrolled)))
