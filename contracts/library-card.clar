;; Library Card Contract
;; Digital library membership system
;; Halal - promoting knowledge
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-EXPIRED (err u405))

(define-data-var member-count uint u0)
(define-data-var total-fees uint u0)

(define-map library-members principal {
  name: (string-utf8 100), membership-type: (string-ascii 20),
  expiry: uint, books-borrowed: uint, max-books: uint, active: bool, joined: uint
})
(define-map membership-tiers (string-ascii 20) { fee: uint, max-books: uint, duration-blocks: uint })

(map-set membership-tiers "basic" { fee: u1000, max-books: u3, duration-blocks: u52560 })
(map-set membership-tiers "standard" { fee: u2500, max-books: u7, duration-blocks: u52560 })
(map-set membership-tiers "premium" { fee: u5000, max-books: u15, duration-blocks: u52560 })

(define-public (register-member (name (string-utf8 100)) (tier (string-ascii 20)))
  (let ((t (unwrap! (map-get? membership-tiers tier) ERR-NOT-FOUND)))
    (try! (stx-transfer? (get fee t) tx-sender CONTRACT-OWNER))
    (map-set library-members tx-sender { name: name, membership-type: tier, expiry: (+ stacks-block-height (get duration-blocks t)), books-borrowed: u0, max-books: (get max-books t), active: true, joined: stacks-block-height })
    (var-set member-count (+ (var-get member-count) u1))
    (var-set total-fees (+ (var-get total-fees) (get fee t))) (ok true)))

(define-public (renew-membership)
  (let (
    (m (unwrap! (map-get? library-members tx-sender) ERR-NOT-FOUND))
    (t (unwrap! (map-get? membership-tiers (get membership-type m)) ERR-NOT-FOUND))
  )
    (try! (stx-transfer? (get fee t) tx-sender CONTRACT-OWNER))
    (map-set library-members tx-sender (merge m { expiry: (+ stacks-block-height (get duration-blocks t)), active: true }))
    (var-set total-fees (+ (var-get total-fees) (get fee t))) (ok true)))

(define-public (borrow-book)
  (let ((m (unwrap! (map-get? library-members tx-sender) ERR-NOT-FOUND)))
    (asserts! (get active m) ERR-NOT-FOUND)
    (asserts! (>= (get expiry m) stacks-block-height) ERR-EXPIRED)
    (asserts! (< (get books-borrowed m) (get max-books m)) ERR-NOT-FOUND)
    (map-set library-members tx-sender (merge m { books-borrowed: (+ (get books-borrowed m) u1) })) (ok true)))

(define-public (return-book)
  (let ((m (unwrap! (map-get? library-members tx-sender) ERR-NOT-FOUND)))
    (asserts! (> (get books-borrowed m) u0) ERR-NOT-FOUND)
    (map-set library-members tx-sender (merge m { books-borrowed: (- (get books-borrowed m) u1) })) (ok true)))

(define-public (upgrade-membership (new-tier (string-ascii 20)))
  (let (
    (m (unwrap! (map-get? library-members tx-sender) ERR-NOT-FOUND))
    (t (unwrap! (map-get? membership-tiers new-tier) ERR-NOT-FOUND))
  )
    (try! (stx-transfer? (get fee t) tx-sender CONTRACT-OWNER))
    (map-set library-members tx-sender (merge m { membership-type: new-tier, max-books: (get max-books t) }))
    (var-set total-fees (+ (var-get total-fees) (get fee t))) (ok true)))

(define-read-only (get-member (who principal)) (map-get? library-members who))
(define-read-only (get-tier (tier (string-ascii 20))) (map-get? membership-tiers tier))
(define-read-only (get-member-count) (ok (var-get member-count)))
(define-read-only (get-total-fees) (ok (var-get total-fees)))
