;; Membership Club Contract
;; On-chain membership management
;; Halal - service-based
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-ALREADY-MEMBER (err u402))
(define-constant ERR-NOT-MEMBER (err u403))
(define-constant ERR-TIER-NOT-FOUND (err u404))

(define-data-var member-count uint u0)
(define-data-var tier-count uint u0)

(define-map tiers uint { name: (string-utf8 50), fee: uint, duration: uint, perks: (string-utf8 200) })
(define-map members principal { tier: uint, joined: uint, expires: uint, referrer: (optional principal) })
(define-map referral-count principal uint)

(define-public (create-tier (name (string-utf8 50)) (fee uint) (duration uint) (perks (string-utf8 200)))
  (let ((id (+ (var-get tier-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set tiers id { name: name, fee: fee, duration: duration, perks: perks })
    (var-set tier-count id)
    (ok id)))

(define-public (join (tier-id uint) (referrer (optional principal)))
  (let ((tier (unwrap! (map-get? tiers tier-id) ERR-TIER-NOT-FOUND)))
    (asserts! (is-none (map-get? members tx-sender)) ERR-ALREADY-MEMBER)
    (try! (stx-transfer? (get fee tier) tx-sender CONTRACT-OWNER))
    (map-set members tx-sender { tier: tier-id, joined: stacks-block-height, expires: (+ stacks-block-height (get duration tier)), referrer: referrer })
    (match referrer ref (map-set referral-count ref (+ (default-to u0 (map-get? referral-count ref)) u1)) true)
    (var-set member-count (+ (var-get member-count) u1))
    (ok true)))

(define-public (renew-membership (tier-id uint))
  (let (
    (tier (unwrap! (map-get? tiers tier-id) ERR-TIER-NOT-FOUND))
    (member (unwrap! (map-get? members tx-sender) ERR-NOT-MEMBER))
  )
    (try! (stx-transfer? (get fee tier) tx-sender CONTRACT-OWNER))
    (map-set members tx-sender (merge member { tier: tier-id, expires: (+ (get expires member) (get duration tier)) }))
    (ok true)))

(define-public (revoke-member (member principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-delete members member)
    (var-set member-count (- (var-get member-count) u1))
    (ok true)))

(define-read-only (get-member (who principal)) (map-get? members who))
(define-read-only (get-tier (id uint)) (map-get? tiers id))
(define-read-only (get-member-count) (ok (var-get member-count)))
(define-read-only (get-tier-count) (ok (var-get tier-count)))
(define-read-only (get-referrals (who principal)) (ok (default-to u0 (map-get? referral-count who))))
(define-read-only (is-active-member (who principal))
  (match (map-get? members who) m (ok (>= (get expires m) stacks-block-height)) (ok false)))
