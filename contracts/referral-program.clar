;; Referral Program Contract
;; Track referrals and distribute rewards
;; Halal - fair commission for service
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-SELF-REFER (err u402))
(define-constant ERR-ALREADY-REFERRED (err u403))
(define-constant REFERRAL-REWARD u500000) ;; 0.5 STX

(define-data-var total-referrals uint u0)
(define-data-var total-rewards-paid uint u0)

(define-map referrers principal { total-referrals: uint, total-earned: uint, active: bool })
(define-map referrals principal principal) ;; referred -> referrer
(define-map referral-codes (string-ascii 20) principal)

(define-public (register-referrer (code (string-ascii 20)))
  (begin
    (asserts! (is-none (map-get? referral-codes code)) ERR-ALREADY-REFERRED)
    (map-set referral-codes code tx-sender)
    (map-set referrers tx-sender { total-referrals: u0, total-earned: u0, active: true })
    (ok true)))

(define-public (use-referral (code (string-ascii 20)))
  (let ((referrer (unwrap! (map-get? referral-codes code) ERR-NOT-AUTHORIZED)))
    (asserts! (not (is-eq tx-sender referrer)) ERR-SELF-REFER)
    (asserts! (is-none (map-get? referrals tx-sender)) ERR-ALREADY-REFERRED)
    (map-set referrals tx-sender referrer)
    (let ((ref-info (default-to { total-referrals: u0, total-earned: u0, active: true } (map-get? referrers referrer))))
      (map-set referrers referrer (merge ref-info { total-referrals: (+ (get total-referrals ref-info) u1) })))
    (var-set total-referrals (+ (var-get total-referrals) u1))
    (ok true)))

(define-public (pay-referral-reward (referrer principal))
  (let ((ref-info (unwrap! (map-get? referrers referrer) ERR-NOT-AUTHORIZED)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? REFERRAL-REWARD tx-sender referrer))
    (map-set referrers referrer (merge ref-info { total-earned: (+ (get total-earned ref-info) REFERRAL-REWARD) }))
    (var-set total-rewards-paid (+ (var-get total-rewards-paid) REFERRAL-REWARD))
    (ok true)))

(define-read-only (get-referrer (who principal)) (map-get? referrers who))
(define-read-only (get-referral-of (who principal)) (map-get? referrals who))
(define-read-only (get-code-owner (code (string-ascii 20))) (map-get? referral-codes code))
(define-read-only (get-total-referrals) (ok (var-get total-referrals)))
(define-read-only (get-total-rewards-paid) (ok (var-get total-rewards-paid)))
