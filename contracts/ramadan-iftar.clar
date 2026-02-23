;; Ramadan Iftar Contract
;; Iftar sponsorship and meal distribution
;; Halal - feeding fasting people
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var iftar-count uint u0)
(define-data-var total-sponsored uint u0)
(define-data-var total-meals uint u0)

(define-map iftars uint { sponsor: principal, location: (string-utf8 100), meals: uint, cost-per-meal: uint, served: uint, date-block: uint, status: (string-ascii 20) })
(define-map iftar-sponsors { iftar-id: uint, sponsor: principal } uint)
(define-map meal-logs { iftar-id: uint, index: uint } { meals-served: uint, block: uint })
(define-map log-count uint uint)

(define-public (create-iftar (location (string-utf8 100)) (meals uint) (cost-per-meal uint) (date-block uint))
  (let (
    (id (+ (var-get iftar-count) u1))
    (total (* meals cost-per-meal))
  )
    (try! (stx-transfer? total tx-sender CONTRACT-OWNER))
    (map-set iftars id { sponsor: tx-sender, location: location, meals: meals, cost-per-meal: cost-per-meal, served: u0, date-block: date-block, status: "scheduled" })
    (var-set iftar-count id)
    (var-set total-sponsored (+ (var-get total-sponsored) total)) (ok id)))

(define-public (co-sponsor (iftar-id uint) (amount uint))
  (let (
    (iftar (unwrap! (map-get? iftars iftar-id) ERR-NOT-FOUND))
    (prev (default-to u0 (map-get? iftar-sponsors { iftar-id: iftar-id, sponsor: tx-sender })))
    (extra-meals (/ amount (get cost-per-meal iftar)))
  )
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set iftar-sponsors { iftar-id: iftar-id, sponsor: tx-sender } (+ prev amount))
    (map-set iftars iftar-id (merge iftar { meals: (+ (get meals iftar) extra-meals) }))
    (var-set total-sponsored (+ (var-get total-sponsored) amount)) (ok extra-meals)))

(define-public (record-meals (iftar-id uint) (meals-served uint))
  (let (
    (iftar (unwrap! (map-get? iftars iftar-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? log-count iftar-id)))
  )
    (asserts! (is-eq tx-sender (get sponsor iftar)) ERR-NOT-AUTHORIZED)
    (map-set meal-logs { iftar-id: iftar-id, index: idx } { meals-served: meals-served, block: stacks-block-height })
    (map-set log-count iftar-id (+ idx u1))
    (map-set iftars iftar-id (merge iftar { served: (+ (get served iftar) meals-served), status: "active" }))
    (var-set total-meals (+ (var-get total-meals) meals-served)) (ok meals-served)))

(define-public (complete-iftar (iftar-id uint))
  (let ((iftar (unwrap! (map-get? iftars iftar-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get sponsor iftar)) ERR-NOT-AUTHORIZED)
    (map-set iftars iftar-id (merge iftar { status: "completed" })) (ok true)))

(define-read-only (get-iftar (id uint)) (map-get? iftars id))
(define-read-only (get-sponsor-amount (iftar-id uint) (who principal)) (ok (default-to u0 (map-get? iftar-sponsors { iftar-id: iftar-id, sponsor: who }))))
(define-read-only (get-meal-log (iftar-id uint) (index uint)) (map-get? meal-logs { iftar-id: iftar-id, index: index }))
(define-read-only (get-iftar-count) (ok (var-get iftar-count)))
(define-read-only (get-total-meals) (ok (var-get total-meals)))
