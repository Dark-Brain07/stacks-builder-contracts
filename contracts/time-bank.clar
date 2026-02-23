;; Time Bank Contract
;; Exchange services based on time units
;; Halal - fair labor exchange
;; Clarity 4 compatible

(define-fungible-token time-credit)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT (err u405))

(define-data-var member-count uint u0)
(define-data-var exchange-count uint u0)

(define-map members principal { name: (string-utf8 50), skills: (string-utf8 200), hours-given: uint, hours-received: uint, joined: uint })
(define-map exchanges uint { provider: principal, receiver: principal, hours: uint, service: (string-utf8 100), block: uint })

(define-public (join-timebank (name (string-utf8 50)) (skills (string-utf8 200)))
  (begin
    (map-set members tx-sender { name: name, skills: skills, hours-given: u0, hours-received: u0, joined: stacks-block-height })
    (try! (ft-mint? time-credit u10 tx-sender))
    (var-set member-count (+ (var-get member-count) u1))
    (ok true)))

(define-public (log-service (receiver principal) (hours uint) (service (string-utf8 100)))
  (let (
    (provider-info (unwrap! (map-get? members tx-sender) ERR-NOT-FOUND))
    (receiver-info (unwrap! (map-get? members receiver) ERR-NOT-FOUND))
    (id (+ (var-get exchange-count) u1))
  )
    (try! (ft-mint? time-credit hours tx-sender))
    (map-set exchanges id { provider: tx-sender, receiver: receiver, hours: hours, service: service, block: stacks-block-height })
    (map-set members tx-sender (merge provider-info { hours-given: (+ (get hours-given provider-info) hours) }))
    (map-set members receiver (merge receiver-info { hours-received: (+ (get hours-received receiver-info) hours) }))
    (var-set exchange-count id)
    (ok id)))

(define-public (spend-credits (amount uint) (to principal))
  (begin
    (asserts! (>= (ft-get-balance time-credit tx-sender) amount) ERR-INSUFFICIENT)
    (try! (ft-transfer? time-credit amount tx-sender to))
    (ok true)))

(define-public (update-skills (skills (string-utf8 200)))
  (let ((member (unwrap! (map-get? members tx-sender) ERR-NOT-FOUND)))
    (map-set members tx-sender (merge member { skills: skills })) (ok true)))

(define-read-only (get-member (who principal)) (map-get? members who))
(define-read-only (get-exchange (id uint)) (map-get? exchanges id))
(define-read-only (get-credit-balance (who principal)) (ok (ft-get-balance time-credit who)))
(define-read-only (get-member-count) (ok (var-get member-count)))
(define-read-only (get-exchange-count) (ok (var-get exchange-count)))
