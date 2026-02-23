;; Energy Credit Contract
;; Renewable energy credit tracking
;; Halal - environmental stewardship
;; Clarity 4 compatible

(define-fungible-token energy-credit)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT (err u405))
(define-constant ERR-NOT-PRODUCER (err u406))

(define-data-var total-minted uint u0)
(define-data-var total-retired uint u0)
(define-data-var producer-count uint u0)

(define-map producers principal { name: (string-utf8 100), energy-type: (string-ascii 20), credits-issued: uint, verified: bool })
(define-map retired-credits { holder: principal, index: uint } { amount: uint, reason: (string-utf8 100), block: uint })
(define-map retirement-count principal uint)

(define-public (register-producer (name (string-utf8 100)) (energy-type (string-ascii 20)))
  (begin (map-set producers tx-sender { name: name, energy-type: energy-type, credits-issued: u0, verified: false })
    (var-set producer-count (+ (var-get producer-count) u1)) (ok true)))

(define-public (verify-producer (producer principal))
  (let ((p (unwrap! (map-get? producers producer) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set producers producer (merge p { verified: true })) (ok true)))

(define-public (mint-credits (amount uint))
  (let ((p (unwrap! (map-get? producers tx-sender) ERR-NOT-PRODUCER)))
    (asserts! (get verified p) ERR-NOT-PRODUCER)
    (try! (ft-mint? energy-credit amount tx-sender))
    (map-set producers tx-sender (merge p { credits-issued: (+ (get credits-issued p) amount) }))
    (var-set total-minted (+ (var-get total-minted) amount))
    (ok amount)))

(define-public (transfer-credits (amount uint) (to principal))
  (ft-transfer? energy-credit amount tx-sender to))

(define-public (retire-credits (amount uint) (reason (string-utf8 100)))
  (let ((idx (default-to u0 (map-get? retirement-count tx-sender))))
    (asserts! (>= (ft-get-balance energy-credit tx-sender) amount) ERR-INSUFFICIENT)
    (try! (ft-burn? energy-credit amount tx-sender))
    (map-set retired-credits { holder: tx-sender, index: idx } { amount: amount, reason: reason, block: stacks-block-height })
    (map-set retirement-count tx-sender (+ idx u1))
    (var-set total-retired (+ (var-get total-retired) amount))
    (ok amount)))

(define-read-only (get-balance (who principal)) (ok (ft-get-balance energy-credit who)))
(define-read-only (get-producer (who principal)) (map-get? producers who))
(define-read-only (get-retired (who principal) (index uint)) (map-get? retired-credits { holder: who, index: index }))
(define-read-only (get-total-minted) (ok (var-get total-minted)))
(define-read-only (get-total-retired) (ok (var-get total-retired)))
