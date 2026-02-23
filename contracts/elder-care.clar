;; Elder Care Contract
;; Elder care coordination and support
;; Halal - honoring elders (birr al-walidayn)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var elder-count uint u0)
(define-data-var visit-count uint u0)
(define-data-var total-support uint u0)

(define-map elders uint { name: (string-utf8 100), age: uint, needs: (string-utf8 200), caregiver: (optional principal), support-received: uint, status: (string-ascii 20) })
(define-map caregivers principal { name: (string-utf8 50), elders-assigned: uint, visits: uint, active: bool })
(define-map visits uint { elder-id: uint, caregiver: principal, notes: (string-utf8 200), block: uint })

(define-public (register-elder (name (string-utf8 100)) (age uint) (needs (string-utf8 200)))
  (let ((id (+ (var-get elder-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set elders id { name: name, age: age, needs: needs, caregiver: none, support-received: u0, status: "active" })
    (var-set elder-count id) (ok id)))

(define-public (register-caregiver (name (string-utf8 50)))
  (begin (map-set caregivers tx-sender { name: name, elders-assigned: u0, visits: u0, active: true }) (ok true)))

(define-public (assign-caregiver (elder-id uint) (caregiver principal))
  (let (
    (elder (unwrap! (map-get? elders elder-id) ERR-NOT-FOUND))
    (cg (unwrap! (map-get? caregivers caregiver) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set elders elder-id (merge elder { caregiver: (some caregiver) }))
    (map-set caregivers caregiver (merge cg { elders-assigned: (+ (get elders-assigned cg) u1) }))
    (ok true)))

(define-public (log-visit (elder-id uint) (notes (string-utf8 200)))
  (let (
    (elder (unwrap! (map-get? elders elder-id) ERR-NOT-FOUND))
    (cg (unwrap! (map-get? caregivers tx-sender) ERR-NOT-AUTHORIZED))
    (vid (+ (var-get visit-count) u1))
  )
    (map-set visits vid { elder-id: elder-id, caregiver: tx-sender, notes: notes, block: stacks-block-height })
    (map-set caregivers tx-sender (merge cg { visits: (+ (get visits cg) u1) }))
    (var-set visit-count vid) (ok vid)))

(define-public (send-support (elder-id uint) (amount uint))
  (let ((elder (unwrap! (map-get? elders elder-id) ERR-NOT-FOUND)))
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set elders elder-id (merge elder { support-received: (+ (get support-received elder) amount) }))
    (var-set total-support (+ (var-get total-support) amount))
    (ok amount)))

(define-read-only (get-elder (id uint)) (map-get? elders id))
(define-read-only (get-caregiver (who principal)) (map-get? caregivers who))
(define-read-only (get-visit (id uint)) (map-get? visits id))
(define-read-only (get-elder-count) (ok (var-get elder-count)))
(define-read-only (get-total-support) (ok (var-get total-support)))
