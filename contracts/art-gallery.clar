;; Art Gallery Contract
;; Digital art exhibition and sales
;; Halal - creative commerce
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-NOT-FOR-SALE (err u405))
(define-constant GALLERY-FEE-PCT u5) ;; 5%

(define-data-var artwork-count uint u0)
(define-data-var total-sales uint u0)
(define-data-var exhibition-count uint u0)

(define-map artworks uint { artist: principal, title: (string-utf8 100), medium: (string-utf8 50), price: uint, for-sale: bool, owner: principal, sold-count: uint })
(define-map exhibitions uint { name: (string-utf8 100), curator: principal, artworks-in: uint, start: uint, end: uint, status: (string-ascii 20) })
(define-map exhibition-artworks { exhibition-id: uint, index: uint } uint)
(define-map artist-profiles principal { name: (string-utf8 100), works: uint, sales: uint, earnings: uint })

(define-public (register-artist (name (string-utf8 100)))
  (begin (map-set artist-profiles tx-sender { name: name, works: u0, sales: u0, earnings: u0 }) (ok true)))

(define-public (create-artwork (title (string-utf8 100)) (medium (string-utf8 50)) (price uint))
  (let (
    (id (+ (var-get artwork-count) u1))
    (profile (default-to { name: u"", works: u0, sales: u0, earnings: u0 } (map-get? artist-profiles tx-sender)))
  )
    (map-set artworks id { artist: tx-sender, title: title, medium: medium, price: price, for-sale: true, owner: tx-sender, sold-count: u0 })
    (map-set artist-profiles tx-sender (merge profile { works: (+ (get works profile) u1) }))
    (var-set artwork-count id) (ok id)))

(define-public (purchase-artwork (artwork-id uint))
  (let (
    (art (unwrap! (map-get? artworks artwork-id) ERR-NOT-FOUND))
    (fee (/ (* (get price art) GALLERY-FEE-PCT) u100))
    (artist-pay (- (get price art) fee))
    (profile (default-to { name: u"", works: u0, sales: u0, earnings: u0 } (map-get? artist-profiles (get artist art))))
  )
    (asserts! (get for-sale art) ERR-NOT-FOR-SALE)
    (try! (stx-transfer? artist-pay tx-sender (get artist art)))
    (try! (stx-transfer? fee tx-sender CONTRACT-OWNER))
    (map-set artworks artwork-id (merge art { owner: tx-sender, for-sale: false, sold-count: (+ (get sold-count art) u1) }))
    (map-set artist-profiles (get artist art) (merge profile { sales: (+ (get sales profile) u1), earnings: (+ (get earnings profile) artist-pay) }))
    (var-set total-sales (+ (var-get total-sales) (get price art)))
    (ok (get price art))))

(define-public (create-exhibition (name (string-utf8 100)) (duration uint))
  (let ((id (+ (var-get exhibition-count) u1)))
    (map-set exhibitions id { name: name, curator: tx-sender, artworks-in: u0, start: stacks-block-height, end: (+ stacks-block-height duration), status: "open" })
    (var-set exhibition-count id) (ok id)))

(define-public (add-to-exhibition (exhibition-id uint) (artwork-id uint))
  (let ((ex (unwrap! (map-get? exhibitions exhibition-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get curator ex)) ERR-NOT-AUTHORIZED)
    (map-set exhibition-artworks { exhibition-id: exhibition-id, index: (get artworks-in ex) } artwork-id)
    (map-set exhibitions exhibition-id (merge ex { artworks-in: (+ (get artworks-in ex) u1) }))
    (ok true)))

(define-read-only (get-artwork (id uint)) (map-get? artworks id))
(define-read-only (get-artist (who principal)) (map-get? artist-profiles who))
(define-read-only (get-exhibition (id uint)) (map-get? exhibitions id))
(define-read-only (get-artwork-count) (ok (var-get artwork-count)))
(define-read-only (get-total-sales) (ok (var-get total-sales)))
