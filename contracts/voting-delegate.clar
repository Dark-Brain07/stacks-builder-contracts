;; Voting Delegate Contract
;; Delegate voting power to representatives
;; Halal - shura (consultation) with delegation
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-VOTED (err u405))
(define-constant ERR-SELF-DELEGATE (err u406))

(define-data-var proposal-count uint u0)

(define-map delegation principal principal)
(define-map voting-power principal uint)
(define-map proposals uint { title: (string-utf8 100), creator: principal, yes-power: uint, no-power: uint, end-block: uint, status: (string-ascii 20) })
(define-map proposal-votes { proposal-id: uint, voter: principal } bool)

(define-public (register-voter (power uint))
  (begin
    (map-set voting-power tx-sender power) (ok true)))

(define-public (delegate-to (delegate principal))
  (let ((my-power (default-to u0 (map-get? voting-power tx-sender))))
    (asserts! (not (is-eq tx-sender delegate)) ERR-SELF-DELEGATE)
    (map-set delegation tx-sender delegate)
    (map-set voting-power delegate (+ (default-to u0 (map-get? voting-power delegate)) my-power))
    (map-set voting-power tx-sender u0)
    (ok true)))

(define-public (revoke-delegation)
  (let (
    (delegate (unwrap! (map-get? delegation tx-sender) ERR-NOT-FOUND))
    (my-power (default-to u1 (map-get? voting-power tx-sender)))
  )
    (map-delete delegation tx-sender)
    (map-set voting-power tx-sender my-power)
    (ok true)))

(define-public (create-proposal (title (string-utf8 100)) (duration uint))
  (let ((id (+ (var-get proposal-count) u1)))
    (map-set proposals id { title: title, creator: tx-sender, yes-power: u0, no-power: u0, end-block: (+ stacks-block-height duration), status: "active" })
    (var-set proposal-count id) (ok id)))

(define-public (cast-vote (proposal-id uint) (support bool))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) ERR-NOT-FOUND))
    (power (default-to u1 (map-get? voting-power tx-sender)))
  )
    (asserts! (< stacks-block-height (get end-block proposal)) ERR-NOT-FOUND)
    (asserts! (is-none (map-get? proposal-votes { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    (map-set proposal-votes { proposal-id: proposal-id, voter: tx-sender } true)
    (if support
      (map-set proposals proposal-id (merge proposal { yes-power: (+ (get yes-power proposal) power) }))
      (map-set proposals proposal-id (merge proposal { no-power: (+ (get no-power proposal) power) })))
    (ok power)))

(define-read-only (get-proposal (id uint)) (map-get? proposals id))
(define-read-only (get-power (who principal)) (ok (default-to u0 (map-get? voting-power who))))
(define-read-only (get-delegate (who principal)) (map-get? delegation who))
(define-read-only (get-proposal-count) (ok (var-get proposal-count)))
(define-read-only (has-voted (proposal-id uint) (voter principal)) (ok (is-some (map-get? proposal-votes { proposal-id: proposal-id, voter: voter }))))
