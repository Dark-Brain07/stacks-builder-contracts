;; Halal Fund Contract
;; Shariah-compliant investment fund
;; Only invests in halal assets
;; Clarity 4 compatible

(define-fungible-token fund-share)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT (err u405))
(define-constant ERR-NOT-SCREENER (err u406))
(define-constant MANAGEMENT-FEE u10) ;; 1% = 10/1000

(define-data-var nav uint u1000000) ;; Net asset value per share (6 decimals)
(define-data-var total-assets uint u0)
(define-data-var asset-count uint u0)

(define-map screeners principal bool)
(define-map fund-assets uint { name: (string-utf8 100), category: (string-ascii 20), value: uint, halal-certified: bool })
(define-map investor-info principal { shares: uint, invested: uint, joined: uint })

(map-set screeners CONTRACT-OWNER true)

(define-public (add-screener (s principal))
  (begin (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED) (map-set screeners s true) (ok true)))

(define-public (invest (amount uint))
  (let (
    (shares (/ (* amount u1000000) (var-get nav)))
    (prev (default-to { shares: u0, invested: u0, joined: u0 } (map-get? investor-info tx-sender)))
  )
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (try! (ft-mint? fund-share shares tx-sender))
    (map-set investor-info tx-sender { shares: (+ (get shares prev) shares), invested: (+ (get invested prev) amount), joined: (if (is-eq (get joined prev) u0) stacks-block-height (get joined prev)) })
    (var-set total-assets (+ (var-get total-assets) amount))
    (ok shares)))

(define-public (redeem (shares uint))
  (let (
    (value (/ (* shares (var-get nav)) u1000000))
    (inv (unwrap! (map-get? investor-info tx-sender) ERR-NOT-FOUND))
  )
    (asserts! (>= (get shares inv) shares) ERR-INSUFFICIENT)
    (try! (ft-burn? fund-share shares tx-sender))
    (try! (stx-transfer? value CONTRACT-OWNER tx-sender))
    (map-set investor-info tx-sender (merge inv { shares: (- (get shares inv) shares) }))
    (var-set total-assets (- (var-get total-assets) value))
    (ok value)))

(define-public (add-asset (name (string-utf8 100)) (category (string-ascii 20)) (value uint))
  (let ((id (+ (var-get asset-count) u1)))
    (asserts! (default-to false (map-get? screeners tx-sender)) ERR-NOT-SCREENER)
    (map-set fund-assets id { name: name, category: category, value: value, halal-certified: true })
    (var-set asset-count id) (ok id)))

(define-public (update-nav (new-nav uint))
  (begin (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED) (var-set nav new-nav) (ok new-nav)))

(define-read-only (get-nav) (ok (var-get nav)))
(define-read-only (get-total-assets) (ok (var-get total-assets)))
(define-read-only (get-investor (who principal)) (map-get? investor-info who))
(define-read-only (get-asset (id uint)) (map-get? fund-assets id))
(define-read-only (get-share-balance (who principal)) (ok (ft-get-balance fund-share who)))
(define-read-only (get-total-shares) (ok (ft-get-supply fund-share)))
