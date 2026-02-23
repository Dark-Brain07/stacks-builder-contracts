;; Impact Bond Contract
;; Social impact bond - pay for outcomes
;; Halal - ethical investment
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-NOT-VERIFIED (err u405))

(define-data-var bond-count uint u0)
(define-data-var total-invested uint u0)

(define-map bonds uint {
  issuer: principal, title: (string-utf8 100), target-outcome: (string-utf8 200),
  total-investment: uint, investor-count: uint, outcome-achieved: bool,
  return-rate: uint, verifier: principal, status: (string-ascii 20), created: uint
})
(define-map investors { bond-id: uint, investor: principal } { amount: uint, returned: bool })

(define-public (issue-bond (title (string-utf8 100)) (outcome (string-utf8 200)) (return-rate uint) (verifier principal))
  (let ((id (+ (var-get bond-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set bonds id { issuer: tx-sender, title: title, target-outcome: outcome, total-investment: u0, investor-count: u0, outcome-achieved: false, return-rate: return-rate, verifier: verifier, status: "open", created: stacks-block-height })
    (var-set bond-count id) (ok id)))

(define-public (invest (bond-id uint) (amount uint))
  (let ((bond (unwrap! (map-get? bonds bond-id) ERR-NOT-FOUND)))
    (asserts! (is-eq (get status bond) "open") ERR-NOT-FOUND)
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set investors { bond-id: bond-id, investor: tx-sender } { amount: amount, returned: false })
    (map-set bonds bond-id (merge bond { total-investment: (+ (get total-investment bond) amount), investor-count: (+ (get investor-count bond) u1) }))
    (var-set total-invested (+ (var-get total-invested) amount))
    (ok amount)))

(define-public (verify-outcome (bond-id uint) (achieved bool))
  (let ((bond (unwrap! (map-get? bonds bond-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get verifier bond)) ERR-NOT-AUTHORIZED)
    (map-set bonds bond-id (merge bond { outcome-achieved: achieved, status: (if achieved "success" "failed") }))
    (ok achieved)))

(define-public (return-investment (bond-id uint) (investor principal))
  (let (
    (bond (unwrap! (map-get? bonds bond-id) ERR-NOT-FOUND))
    (inv (unwrap! (map-get? investors { bond-id: bond-id, investor: investor }) ERR-NOT-FOUND))
    (return-amount (if (get outcome-achieved bond) (+ (get amount inv) (/ (* (get amount inv) (get return-rate bond)) u100)) (get amount inv)))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (not (get returned inv)) ERR-NOT-FOUND)
    (try! (stx-transfer? return-amount tx-sender investor))
    (map-set investors { bond-id: bond-id, investor: investor } (merge inv { returned: true }))
    (ok return-amount)))

(define-read-only (get-bond (id uint)) (map-get? bonds id))
(define-read-only (get-investor (bond-id uint) (investor principal)) (map-get? investors { bond-id: bond-id, investor: investor }))
(define-read-only (get-bond-count) (ok (var-get bond-count)))
(define-read-only (get-total-invested) (ok (var-get total-invested)))
