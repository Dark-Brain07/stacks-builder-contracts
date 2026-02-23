;; Elder Wisdom Contract
;; Elder knowledge sharing platform
;; Halal - honoring elders (birr al-walidayn)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var elder-count uint u0)
(define-data-var story-count uint u0)
(define-data-var total-tips uint u0)

(define-map elders principal { name: (string-utf8 100), expertise: (string-utf8 200), stories-shared: uint, tips-received: uint, registered: uint })
(define-map wisdom-stories uint { elder: principal, title: (string-utf8 100), topic: (string-ascii 20), content-hash: (string-ascii 64), likes: uint, tips: uint, shared: uint })
(define-map story-likes { story-id: uint, liker: principal } bool)
(define-map story-tips { story-id: uint, tipper: principal } uint)

(define-public (register-elder (name (string-utf8 100)) (expertise (string-utf8 200)))
  (begin
    (map-set elders tx-sender { name: name, expertise: expertise, stories-shared: u0, tips-received: u0, registered: stacks-block-height })
    (var-set elder-count (+ (var-get elder-count) u1)) (ok true)))

(define-public (share-wisdom (title (string-utf8 100)) (topic (string-ascii 20)) (content-hash (string-ascii 64)))
  (let (
    (e (unwrap! (map-get? elders tx-sender) ERR-NOT-FOUND))
    (id (+ (var-get story-count) u1))
  )
    (map-set wisdom-stories id { elder: tx-sender, title: title, topic: topic, content-hash: content-hash, likes: u0, tips: u0, shared: stacks-block-height })
    (map-set elders tx-sender (merge e { stories-shared: (+ (get stories-shared e) u1) }))
    (var-set story-count id) (ok id)))

(define-public (like-story (story-id uint))
  (let ((s (unwrap! (map-get? wisdom-stories story-id) ERR-NOT-FOUND)))
    (map-set story-likes { story-id: story-id, liker: tx-sender } true)
    (map-set wisdom-stories story-id (merge s { likes: (+ (get likes s) u1) })) (ok true)))

(define-public (tip-elder (story-id uint) (amount uint))
  (let (
    (s (unwrap! (map-get? wisdom-stories story-id) ERR-NOT-FOUND))
    (e (unwrap! (map-get? elders (get elder s)) ERR-NOT-FOUND))
    (prev (default-to u0 (map-get? story-tips { story-id: story-id, tipper: tx-sender })))
  )
    (try! (stx-transfer? amount tx-sender (get elder s)))
    (map-set story-tips { story-id: story-id, tipper: tx-sender } (+ prev amount))
    (map-set wisdom-stories story-id (merge s { tips: (+ (get tips s) amount) }))
    (map-set elders (get elder s) (merge e { tips-received: (+ (get tips-received e) amount) }))
    (var-set total-tips (+ (var-get total-tips) amount)) (ok amount)))

(define-read-only (get-elder (who principal)) (map-get? elders who))
(define-read-only (get-story (id uint)) (map-get? wisdom-stories id))
(define-read-only (get-like (story-id uint) (who principal)) (map-get? story-likes { story-id: story-id, liker: who }))
(define-read-only (get-elder-count) (ok (var-get elder-count)))
(define-read-only (get-story-count) (ok (var-get story-count)))
