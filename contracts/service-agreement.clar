;; Service Agreement Contract
;; On-chain service level agreements
;; Halal - fair contracts
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-SIGNED (err u405))

(define-data-var agreement-count uint u0)

(define-map agreements uint {
  service-provider: principal, client: principal, terms: (string-utf8 200),
  fee: uint, duration: uint, start-block: uint,
  provider-signed: bool, client-signed: bool, status: (string-ascii 20)
})
(define-map agreement-reviews uint { reviewer: principal, rating: uint, comment: (string-utf8 200), block: uint })

(define-public (create-agreement (client principal) (terms (string-utf8 200)) (fee uint) (duration uint))
  (let ((id (+ (var-get agreement-count) u1)))
    (map-set agreements id {
      service-provider: tx-sender, client: client, terms: terms,
      fee: fee, duration: duration, start-block: u0,
      provider-signed: true, client-signed: false, status: "draft"
    })
    (var-set agreement-count id) (ok id)))

(define-public (sign-agreement (agreement-id uint))
  (let ((agr (unwrap! (map-get? agreements agreement-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get client agr)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get client-signed agr)) ERR-ALREADY-SIGNED)
    (try! (stx-transfer? (get fee agr) tx-sender CONTRACT-OWNER))
    (map-set agreements agreement-id (merge agr { client-signed: true, start-block: stacks-block-height, status: "active" }))
    (ok true)))

(define-public (complete-agreement (agreement-id uint))
  (let ((agr (unwrap! (map-get? agreements agreement-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get client agr)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status agr) "active") ERR-NOT-FOUND)
    (try! (stx-transfer? (get fee agr) CONTRACT-OWNER (get service-provider agr)))
    (map-set agreements agreement-id (merge agr { status: "completed" }))
    (ok true)))

(define-public (dispute-agreement (agreement-id uint))
  (let ((agr (unwrap! (map-get? agreements agreement-id) ERR-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender (get client agr)) (is-eq tx-sender (get service-provider agr))) ERR-NOT-AUTHORIZED)
    (map-set agreements agreement-id (merge agr { status: "disputed" })) (ok true)))

(define-public (resolve-dispute (agreement-id uint) (pay-provider bool))
  (let ((agr (unwrap! (map-get? agreements agreement-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (if pay-provider
      (begin (try! (stx-transfer? (get fee agr) CONTRACT-OWNER (get service-provider agr)))
        (map-set agreements agreement-id (merge agr { status: "resolved-provider" })))
      (begin (try! (stx-transfer? (get fee agr) CONTRACT-OWNER (get client agr)))
        (map-set agreements agreement-id (merge agr { status: "resolved-client" }))))
    (ok true)))

(define-public (leave-review (agreement-id uint) (rating uint) (comment (string-utf8 200)))
  (let ((agr (unwrap! (map-get? agreements agreement-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get client agr)) ERR-NOT-AUTHORIZED)
    (map-set agreement-reviews agreement-id { reviewer: tx-sender, rating: rating, comment: comment, block: stacks-block-height })
    (ok true)))

(define-read-only (get-agreement (id uint)) (map-get? agreements id))
(define-read-only (get-review (id uint)) (map-get? agreement-reviews id))
(define-read-only (get-agreement-count) (ok (var-get agreement-count)))
