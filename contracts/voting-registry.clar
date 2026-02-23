;; Voting Registry Contract
;; Voter registration and election management
;; Halal - democratic governance
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-ALREADY-REGISTERED (err u402))
(define-constant ERR-NOT-REGISTERED (err u403))
(define-constant ERR-ELECTION-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-VOTED (err u405))
(define-constant ERR-ELECTION-CLOSED (err u406))

(define-data-var voter-count uint u0)
(define-data-var election-count uint u0)

(define-map voters principal { registered: uint, elections-voted: uint })
(define-map elections uint { title: (string-utf8 100), options: uint, end-block: uint, total-votes: uint, active: bool })
(define-map option-votes { election: uint, option: uint } uint)
(define-map has-voted { election: uint, voter: principal } bool)

(define-public (register-voter)
  (begin
    (asserts! (is-none (map-get? voters tx-sender)) ERR-ALREADY-REGISTERED)
    (map-set voters tx-sender { registered: stacks-block-height, elections-voted: u0 })
    (var-set voter-count (+ (var-get voter-count) u1)) (ok true)))

(define-public (create-election (title (string-utf8 100)) (num-options uint) (duration uint))
  (let ((id (+ (var-get election-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set elections id { title: title, options: num-options, end-block: (+ stacks-block-height duration), total-votes: u0, active: true })
    (var-set election-count id) (ok id)))

(define-public (cast-vote (election-id uint) (option uint))
  (let (
    (election (unwrap! (map-get? elections election-id) ERR-ELECTION-NOT-FOUND))
    (voter (unwrap! (map-get? voters tx-sender) ERR-NOT-REGISTERED))
    (prev (default-to u0 (map-get? option-votes { election: election-id, option: option })))
  )
    (asserts! (get active election) ERR-ELECTION-CLOSED)
    (asserts! (< stacks-block-height (get end-block election)) ERR-ELECTION-CLOSED)
    (asserts! (is-none (map-get? has-voted { election: election-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    (map-set has-voted { election: election-id, voter: tx-sender } true)
    (map-set option-votes { election: election-id, option: option } (+ prev u1))
    (map-set elections election-id (merge election { total-votes: (+ (get total-votes election) u1) }))
    (map-set voters tx-sender (merge voter { elections-voted: (+ (get elections-voted voter) u1) }))
    (ok true)))

(define-public (close-election (election-id uint))
  (let ((election (unwrap! (map-get? elections election-id) ERR-ELECTION-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set elections election-id (merge election { active: false })) (ok true)))

(define-read-only (get-election (id uint)) (map-get? elections id))
(define-read-only (get-option-votes (election uint) (option uint)) (ok (default-to u0 (map-get? option-votes { election: election, option: option }))))
(define-read-only (get-voter (who principal)) (map-get? voters who))
(define-read-only (get-voter-count) (ok (var-get voter-count)))
(define-read-only (get-election-count) (ok (var-get election-count)))
