;; Blood Bank Contract
;; Blood donation tracking and inventory
;; Halal - saving lives
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-OUT-OF-STOCK (err u405))

(define-data-var donation-count uint u0)
(define-data-var request-count uint u0)

(define-map donors principal { blood-type: (string-ascii 5), donations: uint, last-donation: uint, eligible: bool })
(define-map blood-inventory (string-ascii 5) uint)
(define-map blood-donations uint { donor: principal, blood-type: (string-ascii 5), units: uint, block: uint })
(define-map blood-requests uint { requester: principal, blood-type: (string-ascii 5), units: uint, urgency: uint, status: (string-ascii 20), block: uint })

(define-public (register-donor (blood-type (string-ascii 5)))
  (begin
    (map-set donors tx-sender { blood-type: blood-type, donations: u0, last-donation: u0, eligible: true })
    (ok true)))

(define-public (donate-blood (units uint))
  (let (
    (donor (unwrap! (map-get? donors tx-sender) ERR-NOT-FOUND))
    (did (+ (var-get donation-count) u1))
    (current-inv (default-to u0 (map-get? blood-inventory (get blood-type donor))))
  )
    (asserts! (get eligible donor) ERR-NOT-AUTHORIZED)
    (map-set blood-donations did { donor: tx-sender, blood-type: (get blood-type donor), units: units, block: stacks-block-height })
    (map-set blood-inventory (get blood-type donor) (+ current-inv units))
    (map-set donors tx-sender (merge donor { donations: (+ (get donations donor) u1), last-donation: stacks-block-height }))
    (var-set donation-count did) (ok did)))

(define-public (request-blood (blood-type (string-ascii 5)) (units uint) (urgency uint))
  (let ((rid (+ (var-get request-count) u1)))
    (map-set blood-requests rid { requester: tx-sender, blood-type: blood-type, units: units, urgency: urgency, status: "pending", block: stacks-block-height })
    (var-set request-count rid) (ok rid)))

(define-public (fulfill-request (request-id uint))
  (let (
    (req (unwrap! (map-get? blood-requests request-id) ERR-NOT-FOUND))
    (inv (default-to u0 (map-get? blood-inventory (get blood-type req))))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (>= inv (get units req)) ERR-OUT-OF-STOCK)
    (map-set blood-inventory (get blood-type req) (- inv (get units req)))
    (map-set blood-requests request-id (merge req { status: "fulfilled" }))
    (ok true)))

(define-public (set-eligibility (donor principal) (eligible bool))
  (let ((d (unwrap! (map-get? donors donor) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set donors donor (merge d { eligible: eligible })) (ok true)))

(define-read-only (get-donor (who principal)) (map-get? donors who))
(define-read-only (get-inventory (blood-type (string-ascii 5))) (ok (default-to u0 (map-get? blood-inventory blood-type))))
(define-read-only (get-donation (id uint)) (map-get? blood-donations id))
(define-read-only (get-request (id uint)) (map-get? blood-requests id))
(define-read-only (get-donation-count) (ok (var-get donation-count)))
(define-read-only (get-request-count) (ok (var-get request-count)))
