;; Gift Card Contract
;; Digital gift card issuance and redemption
;; Halal - gift giving
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-REDEEMED (err u405))
(define-constant ERR-EXPIRED (err u406))
(define-constant ERR-INSUFFICIENT (err u407))

(define-data-var card-count uint u0)
(define-data-var total-issued uint u0)

(define-map gift-cards uint {
  creator: principal, recipient: principal, balance: uint,
  message: (string-utf8 100), created: uint, expires: uint, redeemed: bool
})

(define-public (create-gift-card (recipient principal) (amount uint) (message (string-utf8 100)) (validity uint))
  (let ((id (+ (var-get card-count) u1)))
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set gift-cards id {
      creator: tx-sender, recipient: recipient, balance: amount,
      message: message, created: stacks-block-height, expires: (+ stacks-block-height validity), redeemed: false
    })
    (var-set card-count id)
    (var-set total-issued (+ (var-get total-issued) amount))
    (print { event: "gift-card-created", id: id, to: recipient, amount: amount })
    (ok id)))

(define-public (redeem-gift-card (card-id uint))
  (let ((card (unwrap! (map-get? gift-cards card-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get recipient card)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get redeemed card)) ERR-ALREADY-REDEEMED)
    (asserts! (< stacks-block-height (get expires card)) ERR-EXPIRED)
    (try! (stx-transfer? (get balance card) CONTRACT-OWNER tx-sender))
    (map-set gift-cards card-id (merge card { redeemed: true, balance: u0 }))
    (ok (get balance card))))

(define-public (partial-redeem (card-id uint) (amount uint))
  (let ((card (unwrap! (map-get? gift-cards card-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get recipient card)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get redeemed card)) ERR-ALREADY-REDEEMED)
    (asserts! (< stacks-block-height (get expires card)) ERR-EXPIRED)
    (asserts! (<= amount (get balance card)) ERR-INSUFFICIENT)
    (try! (stx-transfer? amount CONTRACT-OWNER tx-sender))
    (map-set gift-cards card-id (merge card { balance: (- (get balance card) amount) }))
    (ok (- (get balance card) amount))))

(define-read-only (get-gift-card (id uint)) (map-get? gift-cards id))
(define-read-only (get-card-count) (ok (var-get card-count)))
(define-read-only (get-total-issued) (ok (var-get total-issued)))
(define-read-only (is-valid (id uint))
  (match (map-get? gift-cards id) c (ok (and (not (get redeemed c)) (< stacks-block-height (get expires c)))) (ok false)))
