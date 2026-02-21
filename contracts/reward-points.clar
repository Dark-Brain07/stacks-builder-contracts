;; Reward Points Contract
;; Loyalty points system with earning and redeeming
;; Built by rajuice for Stacks Builder Rewards
;; Clarity 4 compatible (no as-contract)

(define-fungible-token reward-points)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-INSUFFICIENT-POINTS (err u402))
(define-constant ERR-NOT-MERCHANT (err u403))
(define-constant ERR-REWARD-NOT-FOUND (err u404))
(define-constant POINTS-PER-STX u100)

(define-data-var reward-count uint u0)
(define-data-var total-stx-collected uint u0)

(define-map merchants principal bool)
(define-map rewards uint {
  name: (string-utf8 50),
  cost: uint,
  active: bool
})

;; Initialize owner as merchant
(map-set merchants CONTRACT-OWNER true)

(define-public (add-merchant (merchant principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set merchants merchant true)
    (ok true)))

;; Earn points by spending STX
(define-public (earn-points (amount uint))
  (let ((points (* amount POINTS-PER-STX)))
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (try! (ft-mint? reward-points points tx-sender))
    (var-set total-stx-collected (+ (var-get total-stx-collected) amount))
    (ok points)))

;; Merchant awards bonus points
(define-public (award-points (recipient principal) (points uint))
  (begin
    (asserts! (is-merchant tx-sender) ERR-NOT-MERCHANT)
    (try! (ft-mint? reward-points points recipient))
    (ok points)))

;; Transfer points
(define-public (transfer-points (amount uint) (recipient principal))
  (begin
    (asserts! (> amount u0) ERR-INSUFFICIENT-POINTS)
    (try! (ft-transfer? reward-points amount tx-sender recipient))
    (ok true)))

;; Create a reward
(define-public (create-reward (name (string-utf8 50)) (cost uint))
  (let ((rid (+ (var-get reward-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set rewards rid { name: name, cost: cost, active: true })
    (var-set reward-count rid)
    (ok rid)))

;; Redeem a reward by burning points
(define-public (redeem-reward (reward-id uint))
  (let ((reward (unwrap! (map-get? rewards reward-id) ERR-REWARD-NOT-FOUND)))
    (asserts! (get active reward) ERR-REWARD-NOT-FOUND)
    (asserts! (>= (ft-get-balance reward-points tx-sender) (get cost reward)) ERR-INSUFFICIENT-POINTS)
    (try! (ft-burn? reward-points (get cost reward) tx-sender))
    (print { event: "reward-redeemed", reward-id: reward-id, user: tx-sender })
    (ok true)))

;; Toggle reward availability
(define-public (toggle-reward (reward-id uint))
  (let ((reward (unwrap! (map-get? rewards reward-id) ERR-REWARD-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set rewards reward-id (merge reward { active: (not (get active reward)) }))
    (ok true)))

(define-private (is-merchant (account principal))
  (default-to false (map-get? merchants account)))

(define-read-only (get-points (who principal))
  (ok (ft-get-balance reward-points who)))

(define-read-only (get-reward (reward-id uint))
  (map-get? rewards reward-id))

(define-read-only (get-reward-count)
  (ok (var-get reward-count)))

(define-read-only (get-total-points)
  (ok (ft-get-supply reward-points)))

(define-read-only (check-merchant (account principal))
  (ok (is-merchant account)))

(define-read-only (get-total-collected)
  (ok (var-get total-stx-collected)))
