;; Event Tickets Contract
;; Issue and manage event tickets as NFTs
;; Halal - entertainment/education services
;; Clarity 4 compatible

(define-non-fungible-token ticket uint)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-SOLD-OUT (err u405))
(define-constant ERR-ALREADY-USED (err u406))

(define-data-var ticket-count uint u0)
(define-data-var event-count uint u0)

(define-map events uint { organizer: principal, name: (string-utf8 100), price: uint, max-tickets: uint, sold: uint, event-block: uint })
(define-map ticket-info uint { event-id: uint, holder: principal, used: bool, purchased-at: uint })

(define-public (create-event (name (string-utf8 100)) (price uint) (max-tickets uint) (event-block uint))
  (let ((id (+ (var-get event-count) u1)))
    (map-set events id { organizer: tx-sender, name: name, price: price, max-tickets: max-tickets, sold: u0, event-block: event-block })
    (var-set event-count id) (ok id)))

(define-public (buy-ticket (event-id uint))
  (let (
    (event (unwrap! (map-get? events event-id) ERR-NOT-FOUND))
    (tid (+ (var-get ticket-count) u1))
  )
    (asserts! (< (get sold event) (get max-tickets event)) ERR-SOLD-OUT)
    (try! (stx-transfer? (get price event) tx-sender (get organizer event)))
    (try! (nft-mint? ticket tid tx-sender))
    (map-set ticket-info tid { event-id: event-id, holder: tx-sender, used: false, purchased-at: stacks-block-height })
    (map-set events event-id (merge event { sold: (+ (get sold event) u1) }))
    (var-set ticket-count tid) (ok tid)))

(define-public (use-ticket (ticket-id uint))
  (let ((info (unwrap! (map-get? ticket-info ticket-id) ERR-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender (get holder info)) (is-eq tx-sender CONTRACT-OWNER)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get used info)) ERR-ALREADY-USED)
    (map-set ticket-info ticket-id (merge info { used: true })) (ok true)))

(define-public (transfer-ticket (ticket-id uint) (to principal))
  (let ((info (unwrap! (map-get? ticket-info ticket-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get holder info)) ERR-NOT-AUTHORIZED)
    (try! (nft-transfer? ticket ticket-id tx-sender to))
    (map-set ticket-info ticket-id (merge info { holder: to })) (ok true)))

(define-read-only (get-event (id uint)) (map-get? events id))
(define-read-only (get-ticket (id uint)) (map-get? ticket-info id))
(define-read-only (get-event-count) (ok (var-get event-count)))
(define-read-only (get-ticket-count) (ok (var-get ticket-count)))
(define-read-only (get-ticket-owner (id uint)) (nft-get-owner? ticket id))
