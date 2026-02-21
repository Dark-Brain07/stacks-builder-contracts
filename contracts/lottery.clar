;; Lottery Contract
;; Decentralized lottery with STX prizes
;; Built by rajuice for Stacks Builder Rewards

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-LOTTERY-CLOSED (err u402))
(define-constant ERR-LOTTERY-ACTIVE (err u403))
(define-constant ERR-ALREADY-ENTERED (err u404))
(define-constant ERR-NO-PLAYERS (err u405))
(define-constant TICKET-PRICE u1000000) ;; 1 STX

(define-data-var round-id uint u0)
(define-data-var is-active bool false)
(define-data-var player-count uint u0)
(define-data-var prize-pool uint u0)

(define-map players { round: uint, index: uint } principal)
(define-map player-entered { round: uint, player: principal } bool)
(define-map round-winners uint principal)

(define-public (start-lottery)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (not (var-get is-active)) ERR-LOTTERY-ACTIVE)
    (var-set round-id (+ (var-get round-id) u1))
    (var-set is-active true)
    (var-set player-count u0)
    (var-set prize-pool u0)
    (ok (var-get round-id))))

(define-public (buy-ticket)
  (let ((round (var-get round-id))
        (index (var-get player-count)))
    (asserts! (var-get is-active) ERR-LOTTERY-CLOSED)
    (asserts! (is-none (map-get? player-entered { round: round, player: tx-sender })) ERR-ALREADY-ENTERED)
    (try! (stx-transfer? TICKET-PRICE tx-sender (as-contract tx-sender)))
    (map-set players { round: round, index: index } tx-sender)
    (map-set player-entered { round: round, player: tx-sender } true)
    (var-set player-count (+ index u1))
    (var-set prize-pool (+ (var-get prize-pool) TICKET-PRICE))
    (ok index)))

(define-public (draw-winner)
  (let (
    (round (var-get round-id))
    (count (var-get player-count))
    (seed (mod (+ stacks-block-height (var-get prize-pool)) count))
    (winner (unwrap! (map-get? players { round: round, index: seed }) ERR-NO-PLAYERS))
    (prize (var-get prize-pool))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (var-get is-active) ERR-LOTTERY-CLOSED)
    (asserts! (> count u0) ERR-NO-PLAYERS)
    (try! (as-contract (stx-transfer? prize tx-sender winner)))
    (map-set round-winners round winner)
    (var-set is-active false)
    (print { event: "winner-drawn", round: round, winner: winner, prize: prize })
    (ok winner)))

(define-read-only (get-round) (ok (var-get round-id)))
(define-read-only (get-player-count) (ok (var-get player-count)))
(define-read-only (get-prize-pool) (ok (var-get prize-pool)))
(define-read-only (get-is-active) (ok (var-get is-active)))
(define-read-only (get-ticket-price) (ok TICKET-PRICE))
(define-read-only (get-winner (round uint)) (map-get? round-winners round))
