;; Charity Donation Contract (Sadaqah)
;; Track and manage charitable donations transparently
;; Halal - promotes charitable giving (sadaqah)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-CAUSE-NOT-FOUND (err u404))
(define-constant ERR-CAUSE-CLOSED (err u405))

(define-data-var cause-count uint u0)
(define-data-var total-donated uint u0)

(define-map causes uint {
  name: (string-utf8 100),
  description: (string-utf8 200),
  recipient: principal,
  raised: uint,
  active: bool,
  created-at: uint
})
(define-map donor-history { cause-id: uint, donor: principal } uint)
(define-map donor-total principal uint)

(define-public (create-cause (name (string-utf8 100)) (description (string-utf8 200)) (recipient principal))
  (let ((cause-id (+ (var-get cause-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set causes cause-id {
      name: name, description: description, recipient: recipient,
      raised: u0, active: true, created-at: stacks-block-height
    })
    (var-set cause-count cause-id)
    (ok cause-id)))

(define-public (donate (cause-id uint) (amount uint))
  (let (
    (cause (unwrap! (map-get? causes cause-id) ERR-CAUSE-NOT-FOUND))
    (prev (default-to u0 (map-get? donor-history { cause-id: cause-id, donor: tx-sender })))
    (prev-total (default-to u0 (map-get? donor-total tx-sender)))
  )
    (asserts! (get active cause) ERR-CAUSE-CLOSED)
    (try! (stx-transfer? amount tx-sender (get recipient cause)))
    (map-set donor-history { cause-id: cause-id, donor: tx-sender } (+ prev amount))
    (map-set donor-total tx-sender (+ prev-total amount))
    (map-set causes cause-id (merge cause { raised: (+ (get raised cause) amount) }))
    (var-set total-donated (+ (var-get total-donated) amount))
    (print { event: "donation", cause: cause-id, donor: tx-sender, amount: amount })
    (ok amount)))

(define-public (close-cause (cause-id uint))
  (let ((cause (unwrap! (map-get? causes cause-id) ERR-CAUSE-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set causes cause-id (merge cause { active: false }))
    (ok true)))

(define-read-only (get-cause (cause-id uint)) (map-get? causes cause-id))
(define-read-only (get-cause-count) (ok (var-get cause-count)))
(define-read-only (get-total-donated) (ok (var-get total-donated)))
(define-read-only (get-donor-amount (cause-id uint) (donor principal))
  (ok (default-to u0 (map-get? donor-history { cause-id: cause-id, donor: donor }))))
(define-read-only (get-donor-total (donor principal))
  (ok (default-to u0 (map-get? donor-total donor))))
