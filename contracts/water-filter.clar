;; Water Filter Contract
;; Water filter distribution program
;; Halal - clean water access (sadaqah jariyah)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var program-count uint u0)
(define-data-var filters-distributed uint u0)
(define-data-var total-funded uint u0)

(define-map programs uint { coordinator: principal, region: (string-utf8 100), filter-cost: uint, goal: uint, distributed: uint, funded: uint, status: (string-ascii 20) })
(define-map distributions { program-id: uint, index: uint } { community: (string-utf8 100), filters: uint, people-served: uint, block: uint })
(define-map dist-count uint uint)
(define-map program-donors { program-id: uint, donor: principal } uint)

(define-public (create-program (region (string-utf8 100)) (filter-cost uint) (goal uint))
  (let ((id (+ (var-get program-count) u1)))
    (map-set programs id { coordinator: tx-sender, region: region, filter-cost: filter-cost, goal: goal, distributed: u0, funded: u0, status: "active" })
    (var-set program-count id) (ok id)))

(define-public (donate-filters (program-id uint) (amount uint))
  (let (
    (p (unwrap! (map-get? programs program-id) ERR-NOT-FOUND))
    (prev (default-to u0 (map-get? program-donors { program-id: program-id, donor: tx-sender })))
  )
    (try! (stx-transfer? amount tx-sender (get coordinator p)))
    (map-set program-donors { program-id: program-id, donor: tx-sender } (+ prev amount))
    (map-set programs program-id (merge p { funded: (+ (get funded p) amount) }))
    (var-set total-funded (+ (var-get total-funded) amount)) (ok amount)))

(define-public (distribute-filters (program-id uint) (community (string-utf8 100)) (filters uint) (people uint))
  (let (
    (p (unwrap! (map-get? programs program-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? dist-count program-id)))
  )
    (asserts! (is-eq tx-sender (get coordinator p)) ERR-NOT-AUTHORIZED)
    (map-set distributions { program-id: program-id, index: idx } { community: community, filters: filters, people-served: people, block: stacks-block-height })
    (map-set dist-count program-id (+ idx u1))
    (map-set programs program-id (merge p { distributed: (+ (get distributed p) filters) }))
    (var-set filters-distributed (+ (var-get filters-distributed) filters)) (ok idx)))

(define-public (close-program (program-id uint))
  (let ((p (unwrap! (map-get? programs program-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get coordinator p)) ERR-NOT-AUTHORIZED)
    (map-set programs program-id (merge p { status: "completed" })) (ok true)))

(define-read-only (get-program (id uint)) (map-get? programs id))
(define-read-only (get-distribution (program-id uint) (index uint)) (map-get? distributions { program-id: program-id, index: index }))
(define-read-only (get-donor (program-id uint) (who principal)) (ok (default-to u0 (map-get? program-donors { program-id: program-id, donor: who }))))
(define-read-only (get-program-count) (ok (var-get program-count)))
(define-read-only (get-filters-distributed) (ok (var-get filters-distributed)))
