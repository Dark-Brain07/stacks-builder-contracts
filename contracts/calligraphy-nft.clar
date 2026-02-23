;; Calligraphy NFT Contract
;; Islamic calligraphy collectible management
;; Halal - Arabic art preservation
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ROYALTY-PCT u5)

(define-data-var artwork-count uint u0)
(define-data-var total-sales uint u0)

(define-map artworks uint { artist: principal, title: (string-utf8 100), script-style: (string-ascii 20), verse-ref: (string-utf8 100), image-hash: (string-ascii 64), owner: principal, price: uint, for-sale: bool, sales: uint, created: uint })
(define-map artist-profiles principal { name: (string-utf8 100), style: (string-utf8 50), works: uint, earned: uint })
(define-map sale-history { artwork-id: uint, index: uint } { from: principal, to: principal, price: uint, block: uint })
(define-map sale-hist-count uint uint)

(define-public (register-artist (name (string-utf8 100)) (style (string-utf8 50)))
  (begin (map-set artist-profiles tx-sender { name: name, style: style, works: u0, earned: u0 }) (ok true)))

(define-public (mint-artwork (title (string-utf8 100)) (script-style (string-ascii 20)) (verse-ref (string-utf8 100)) (image-hash (string-ascii 64)) (price uint))
  (let (
    (id (+ (var-get artwork-count) u1))
    (a (unwrap! (map-get? artist-profiles tx-sender) ERR-NOT-AUTHORIZED))
  )
    (map-set artworks id { artist: tx-sender, title: title, script-style: script-style, verse-ref: verse-ref, image-hash: image-hash, owner: tx-sender, price: price, for-sale: true, sales: u0, created: stacks-block-height })
    (map-set artist-profiles tx-sender (merge a { works: (+ (get works a) u1) }))
    (var-set artwork-count id) (ok id)))

(define-public (buy-artwork (artwork-id uint))
  (let (
    (art (unwrap! (map-get? artworks artwork-id) ERR-NOT-FOUND))
    (royalty (/ (* (get price art) ROYALTY-PCT) u100))
    (seller-pay (- (get price art) royalty))
    (idx (default-to u0 (map-get? sale-hist-count artwork-id)))
    (ap (unwrap! (map-get? artist-profiles (get artist art)) ERR-NOT-FOUND))
  )
    (asserts! (get for-sale art) ERR-NOT-FOUND)
    (try! (stx-transfer? seller-pay tx-sender (get owner art)))
    (try! (stx-transfer? royalty tx-sender (get artist art)))
    (map-set sale-history { artwork-id: artwork-id, index: idx } { from: (get owner art), to: tx-sender, price: (get price art), block: stacks-block-height })
    (map-set sale-hist-count artwork-id (+ idx u1))
    (map-set artworks artwork-id (merge art { owner: tx-sender, for-sale: false, sales: (+ (get sales art) u1) }))
    (map-set artist-profiles (get artist art) (merge ap { earned: (+ (get earned ap) royalty) }))
    (var-set total-sales (+ (var-get total-sales) (get price art))) (ok (get price art))))

(define-public (set-for-sale (artwork-id uint) (price uint))
  (let ((art (unwrap! (map-get? artworks artwork-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner art)) ERR-NOT-AUTHORIZED)
    (map-set artworks artwork-id (merge art { for-sale: true, price: price })) (ok true)))

(define-read-only (get-artwork (id uint)) (map-get? artworks id))
(define-read-only (get-artist (who principal)) (map-get? artist-profiles who))
(define-read-only (get-sale (artwork-id uint) (index uint)) (map-get? sale-history { artwork-id: artwork-id, index: index }))
(define-read-only (get-artwork-count) (ok (var-get artwork-count)))
(define-read-only (get-total-sales) (ok (var-get total-sales)))
