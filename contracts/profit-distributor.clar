;; Profit Distributor Contract
;; Distribute profits proportionally to shareholders
;; Halal - profit-sharing (mudarabah)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-SHAREHOLDER (err u405))

(define-data-var shareholder-count uint u0)
(define-data-var total-shares uint u0)
(define-data-var total-distributed uint u0)
(define-data-var distribution-count uint u0)

(define-map shareholders principal { shares: uint, claimed: uint, joined: uint })
(define-map distributions uint { amount: uint, per-share: uint, block: uint })

(define-public (add-shareholder (holder principal) (shares uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? shareholders holder)) ERR-ALREADY-SHAREHOLDER)
    (map-set shareholders holder { shares: shares, claimed: u0, joined: stacks-block-height })
    (var-set total-shares (+ (var-get total-shares) shares))
    (var-set shareholder-count (+ (var-get shareholder-count) u1)) (ok true)))

(define-public (update-shares (holder principal) (new-shares uint))
  (let ((sh (unwrap! (map-get? shareholders holder) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set total-shares (+ (- (var-get total-shares) (get shares sh)) new-shares))
    (map-set shareholders holder (merge sh { shares: new-shares })) (ok true)))

(define-public (distribute-profit (total-amount uint))
  (let ((id (+ (var-get distribution-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set distributions id { amount: total-amount, per-share: (/ total-amount (var-get total-shares)), block: stacks-block-height })
    (var-set distribution-count id)
    (var-set total-distributed (+ (var-get total-distributed) total-amount))
    (ok id)))

(define-public (pay-shareholder (holder principal) (amount uint))
  (let ((sh (unwrap! (map-get? shareholders holder) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? amount tx-sender holder))
    (map-set shareholders holder (merge sh { claimed: (+ (get claimed sh) amount) }))
    (ok amount)))

(define-read-only (get-shareholder (who principal)) (map-get? shareholders who))
(define-read-only (get-distribution (id uint)) (map-get? distributions id))
(define-read-only (get-total-shares) (ok (var-get total-shares)))
(define-read-only (get-shareholder-count) (ok (var-get shareholder-count)))
(define-read-only (get-total-distributed) (ok (var-get total-distributed)))
(define-read-only (get-share-value (who principal))
  (match (map-get? shareholders who) sh (ok (/ (* (get shares sh) u1000000) (var-get total-shares))) (ok u0)))
