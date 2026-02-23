;; Knowledge Rewards Contract
;; Reward knowledge contributions
;; Halal - spreading knowledge
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var content-count uint u0)
(define-data-var total-rewards uint u0)

(define-map knowledge-entries uint {
  author: principal, title: (string-utf8 100), category: (string-ascii 20),
  content-hash: (buff 32), upvotes: uint, reward-earned: uint, created: uint
})
(define-map author-stats principal { entries: uint, total-upvotes: uint, total-earned: uint })
(define-map has-upvoted { content-id: uint, voter: principal } bool)
(define-data-var reward-per-upvote uint u1000) ;; 0.001 STX

(define-public (publish-entry (title (string-utf8 100)) (category (string-ascii 20)) (content-hash (buff 32)))
  (let (
    (id (+ (var-get content-count) u1))
    (stats (default-to { entries: u0, total-upvotes: u0, total-earned: u0 } (map-get? author-stats tx-sender)))
  )
    (map-set knowledge-entries id { author: tx-sender, title: title, category: category, content-hash: content-hash, upvotes: u0, reward-earned: u0, created: stacks-block-height })
    (map-set author-stats tx-sender (merge stats { entries: (+ (get entries stats) u1) }))
    (var-set content-count id) (ok id)))

(define-public (upvote (content-id uint))
  (let (
    (entry (unwrap! (map-get? knowledge-entries content-id) ERR-NOT-FOUND))
    (reward (var-get reward-per-upvote))
    (stats (default-to { entries: u0, total-upvotes: u0, total-earned: u0 } (map-get? author-stats (get author entry))))
  )
    (asserts! (not (is-eq tx-sender (get author entry))) ERR-NOT-AUTHORIZED)
    (asserts! (not (default-to false (map-get? has-upvoted { content-id: content-id, voter: tx-sender }))) ERR-NOT-AUTHORIZED)
    (map-set has-upvoted { content-id: content-id, voter: tx-sender } true)
    (map-set knowledge-entries content-id (merge entry { upvotes: (+ (get upvotes entry) u1), reward-earned: (+ (get reward-earned entry) reward) }))
    (map-set author-stats (get author entry) (merge stats { total-upvotes: (+ (get total-upvotes stats) u1), total-earned: (+ (get total-earned stats) reward) }))
    (var-set total-rewards (+ (var-get total-rewards) reward))
    (ok true)))

(define-public (claim-rewards (content-id uint))
  (let ((entry (unwrap! (map-get? knowledge-entries content-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> (get reward-earned entry) u0) ERR-NOT-FOUND)
    (try! (stx-transfer? (get reward-earned entry) tx-sender (get author entry)))
    (map-set knowledge-entries content-id (merge entry { reward-earned: u0 })) (ok true)))

(define-public (set-reward-rate (rate uint))
  (begin (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED) (var-set reward-per-upvote rate) (ok rate)))

(define-read-only (get-entry (id uint)) (map-get? knowledge-entries id))
(define-read-only (get-author-stats (who principal)) (map-get? author-stats who))
(define-read-only (get-content-count) (ok (var-get content-count)))
(define-read-only (get-total-rewards) (ok (var-get total-rewards)))
