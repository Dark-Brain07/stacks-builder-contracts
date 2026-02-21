;; Token Vesting Contract
;; Linear vesting schedule for token distribution
;; Built by rajuice for Stacks Builder Rewards
;; Clarity 4 compatible (no as-contract)

(define-fungible-token vested-token)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-SCHEDULE-NOT-FOUND (err u404))
(define-constant ERR-NOTHING-TO-CLAIM (err u405))
(define-constant ERR-ALREADY-REVOKED (err u406))

(define-data-var schedule-count uint u0)

(define-map vesting-schedules uint {
  beneficiary: principal,
  total-amount: uint,
  claimed: uint,
  start-height: uint,
  cliff-height: uint,
  end-height: uint,
  revoked: bool
})

;; Create a vesting schedule and mint tokens to owner for distribution
(define-public (create-vesting (beneficiary principal) (total-amount uint) (cliff-blocks uint) (vesting-blocks uint))
  (let ((schedule-id (+ (var-get schedule-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (try! (ft-mint? vested-token total-amount CONTRACT-OWNER))
    (map-set vesting-schedules schedule-id {
      beneficiary: beneficiary,
      total-amount: total-amount,
      claimed: u0,
      start-height: stacks-block-height,
      cliff-height: (+ stacks-block-height cliff-blocks),
      end-height: (+ stacks-block-height vesting-blocks),
      revoked: false
    })
    (var-set schedule-count schedule-id)
    (ok schedule-id)))

;; Owner distributes vested tokens to beneficiary
(define-public (distribute-vested (schedule-id uint))
  (let (
    (schedule (unwrap! (map-get? vesting-schedules schedule-id) ERR-SCHEDULE-NOT-FOUND))
    (claimable (get-claimable-amount schedule-id))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> claimable u0) ERR-NOTHING-TO-CLAIM)
    (try! (ft-transfer? vested-token claimable CONTRACT-OWNER (get beneficiary schedule)))
    (map-set vesting-schedules schedule-id (merge schedule { claimed: (+ (get claimed schedule) claimable) }))
    (ok claimable)))

;; Revoke vesting
(define-public (revoke-vesting (schedule-id uint))
  (let ((schedule (unwrap! (map-get? vesting-schedules schedule-id) ERR-SCHEDULE-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (not (get revoked schedule)) ERR-ALREADY-REVOKED)
    (map-set vesting-schedules schedule-id (merge schedule { revoked: true }))
    (ok true)))

(define-read-only (get-claimable-amount (schedule-id uint))
  (let ((schedule (default-to {
      beneficiary: CONTRACT-OWNER, total-amount: u0, claimed: u0,
      start-height: u0, cliff-height: u0, end-height: u0, revoked: false
    } (map-get? vesting-schedules schedule-id))))
    (if (or (get revoked schedule) (< stacks-block-height (get cliff-height schedule)))
      u0
      (let (
        (elapsed (- stacks-block-height (get start-height schedule)))
        (duration (- (get end-height schedule) (get start-height schedule)))
        (vested (if (>= stacks-block-height (get end-height schedule))
                  (get total-amount schedule)
                  (/ (* (get total-amount schedule) elapsed) duration)))
      )
        (- vested (get claimed schedule))))))

(define-read-only (get-schedule (schedule-id uint))
  (map-get? vesting-schedules schedule-id))

(define-read-only (get-schedule-count)
  (ok (var-get schedule-count)))

(define-read-only (get-name) (ok "Vested Token"))
(define-read-only (get-symbol) (ok "VST"))
(define-read-only (get-decimals) (ok u6))
(define-read-only (get-balance (who principal)) (ok (ft-get-balance vested-token who)))
(define-read-only (get-total-supply) (ok (ft-get-supply vested-token)))
