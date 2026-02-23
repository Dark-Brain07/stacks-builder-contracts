;; Peer Lending Pool Contract
;; P2P interest-free lending circles
;; Halal - no riba, community-based
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-MEMBER (err u405))
(define-constant ERR-FULL (err u406))

(define-data-var circle-count uint u0)
(define-data-var total-circulated uint u0)

(define-map circles uint {
  name: (string-utf8 100), organizer: principal, contribution: uint,
  max-members: uint, current-members: uint, current-round: uint,
  status: (string-ascii 20), created: uint
})
(define-map circle-members { circle-id: uint, member: principal } { joined: uint, contributed: uint, received: uint })
(define-map round-recipient { circle-id: uint, round: uint } principal)
(define-map member-contributed-round { circle-id: uint, member: principal, round: uint } bool)

(define-public (create-circle (name (string-utf8 100)) (contribution uint) (max-members uint))
  (let ((id (+ (var-get circle-count) u1)))
    (map-set circles id { name: name, organizer: tx-sender, contribution: contribution, max-members: max-members, current-members: u1, current-round: u0, status: "forming", created: stacks-block-height })
    (map-set circle-members { circle-id: id, member: tx-sender } { joined: stacks-block-height, contributed: u0, received: u0 })
    (var-set circle-count id) (ok id)))

(define-public (join-circle (circle-id uint))
  (let ((circle (unwrap! (map-get? circles circle-id) ERR-NOT-FOUND)))
    (asserts! (< (get current-members circle) (get max-members circle)) ERR-FULL)
    (asserts! (is-none (map-get? circle-members { circle-id: circle-id, member: tx-sender })) ERR-ALREADY-MEMBER)
    (map-set circle-members { circle-id: circle-id, member: tx-sender } { joined: stacks-block-height, contributed: u0, received: u0 })
    (map-set circles circle-id (merge circle { current-members: (+ (get current-members circle) u1) }))
    (ok true)))

(define-public (start-round (circle-id uint) (recipient principal))
  (let (
    (circle (unwrap! (map-get? circles circle-id) ERR-NOT-FOUND))
    (round (+ (get current-round circle) u1))
  )
    (asserts! (is-eq tx-sender (get organizer circle)) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? circle-members { circle-id: circle-id, member: recipient })) ERR-NOT-FOUND)
    (map-set round-recipient { circle-id: circle-id, round: round } recipient)
    (map-set circles circle-id (merge circle { current-round: round, status: "active" }))
    (ok round)))

(define-public (contribute (circle-id uint))
  (let (
    (circle (unwrap! (map-get? circles circle-id) ERR-NOT-FOUND))
    (round (get current-round circle))
    (recipient (unwrap! (map-get? round-recipient { circle-id: circle-id, round: round }) ERR-NOT-FOUND))
    (member (unwrap! (map-get? circle-members { circle-id: circle-id, member: tx-sender }) ERR-NOT-FOUND))
  )
    (asserts! (is-none (map-get? member-contributed-round { circle-id: circle-id, member: tx-sender, round: round })) ERR-ALREADY-MEMBER)
    (try! (stx-transfer? (get contribution circle) tx-sender recipient))
    (map-set member-contributed-round { circle-id: circle-id, member: tx-sender, round: round } true)
    (map-set circle-members { circle-id: circle-id, member: tx-sender } (merge member { contributed: (+ (get contributed member) (get contribution circle)) }))
    (var-set total-circulated (+ (var-get total-circulated) (get contribution circle)))
    (ok (get contribution circle))))

(define-read-only (get-circle (id uint)) (map-get? circles id))
(define-read-only (get-member (circle-id uint) (member principal)) (map-get? circle-members { circle-id: circle-id, member: member }))
(define-read-only (get-round-recipient (circle-id uint) (round uint)) (map-get? round-recipient { circle-id: circle-id, round: round }))
(define-read-only (get-circle-count) (ok (var-get circle-count)))
(define-read-only (get-total-circulated) (ok (var-get total-circulated)))
