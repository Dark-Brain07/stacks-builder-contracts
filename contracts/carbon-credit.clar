;; Carbon Credit Contract
;; Track and trade carbon offset credits
;; Halal - environmental stewardship (khalifah)
;; Clarity 4 compatible

(define-fungible-token carbon-credit)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT (err u405))
(define-constant ERR-NOT-VERIFIER (err u406))

(define-data-var total-retired uint u0)
(define-data-var project-count uint u0)

(define-map verifiers principal bool)
(define-map projects uint { name: (string-utf8 100), creator: principal, credits-issued: uint, verified: bool, created: uint })
(define-map retirements { owner: principal, index: uint } { amount: uint, reason: (string-utf8 200), block: uint })
(define-map retirement-count principal uint)

(map-set verifiers CONTRACT-OWNER true)

(define-public (add-verifier (v principal))
  (begin (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED) (map-set verifiers v true) (ok true)))

(define-public (create-project (name (string-utf8 100)))
  (let ((id (+ (var-get project-count) u1)))
    (map-set projects id { name: name, creator: tx-sender, credits-issued: u0, verified: false, created: stacks-block-height })
    (var-set project-count id) (ok id)))

(define-public (verify-and-issue (project-id uint) (credits uint))
  (let ((proj (unwrap! (map-get? projects project-id) ERR-NOT-FOUND)))
    (asserts! (default-to false (map-get? verifiers tx-sender)) ERR-NOT-VERIFIER)
    (try! (ft-mint? carbon-credit credits (get creator proj)))
    (map-set projects project-id (merge proj { credits-issued: (+ (get credits-issued proj) credits), verified: true }))
    (ok credits)))

(define-public (transfer-credits (amount uint) (to principal))
  (ft-transfer? carbon-credit amount tx-sender to))

(define-public (retire-credits (amount uint) (reason (string-utf8 200)))
  (let ((idx (default-to u0 (map-get? retirement-count tx-sender))))
    (try! (ft-burn? carbon-credit amount tx-sender))
    (map-set retirements { owner: tx-sender, index: idx } { amount: amount, reason: reason, block: stacks-block-height })
    (map-set retirement-count tx-sender (+ idx u1))
    (var-set total-retired (+ (var-get total-retired) amount))
    (print { event: "credits-retired", by: tx-sender, amount: amount })
    (ok amount)))

(define-read-only (get-balance (who principal)) (ok (ft-get-balance carbon-credit who)))
(define-read-only (get-project (id uint)) (map-get? projects id))
(define-read-only (get-total-supply) (ok (ft-get-supply carbon-credit)))
(define-read-only (get-total-retired) (ok (var-get total-retired)))
(define-read-only (get-project-count) (ok (var-get project-count)))
(define-read-only (get-retirement (owner principal) (index uint)) (map-get? retirements { owner: owner, index: index }))
