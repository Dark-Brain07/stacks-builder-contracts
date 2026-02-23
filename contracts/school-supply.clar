;; School Supply Contract
;; School supplies donation for needy students
;; Halal - supporting education
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var program-count uint u0)
(define-data-var students-helped uint u0)
(define-data-var total-donated uint u0)

(define-map programs uint { coordinator: principal, school-name: (string-utf8 100), students-target: uint, students-served: uint, budget: uint, spent: uint, status: (string-ascii 20) })
(define-map supply-kits { program-id: uint, index: uint } { items: (string-utf8 200), cost: uint, grade: (string-ascii 10), block: uint })
(define-map kit-count uint uint)
(define-map program-donors { program-id: uint, donor: principal } uint)

(define-public (create-program (school-name (string-utf8 100)) (students-target uint))
  (let ((id (+ (var-get program-count) u1)))
    (map-set programs id { coordinator: tx-sender, school-name: school-name, students-target: students-target, students-served: u0, budget: u0, spent: u0, status: "active" })
    (var-set program-count id) (ok id)))

(define-public (donate-supplies (program-id uint) (amount uint))
  (let (
    (p (unwrap! (map-get? programs program-id) ERR-NOT-FOUND))
    (prev (default-to u0 (map-get? program-donors { program-id: program-id, donor: tx-sender })))
  )
    (try! (stx-transfer? amount tx-sender (get coordinator p)))
    (map-set program-donors { program-id: program-id, donor: tx-sender } (+ prev amount))
    (map-set programs program-id (merge p { budget: (+ (get budget p) amount) }))
    (var-set total-donated (+ (var-get total-donated) amount)) (ok amount)))

(define-public (distribute-kit (program-id uint) (items (string-utf8 200)) (cost uint) (grade (string-ascii 10)))
  (let (
    (p (unwrap! (map-get? programs program-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? kit-count program-id)))
  )
    (asserts! (is-eq tx-sender (get coordinator p)) ERR-NOT-AUTHORIZED)
    (map-set supply-kits { program-id: program-id, index: idx } { items: items, cost: cost, grade: grade, block: stacks-block-height })
    (map-set kit-count program-id (+ idx u1))
    (map-set programs program-id (merge p { students-served: (+ (get students-served p) u1), spent: (+ (get spent p) cost) }))
    (var-set students-helped (+ (var-get students-helped) u1)) (ok idx)))

(define-public (close-program (program-id uint))
  (let ((p (unwrap! (map-get? programs program-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get coordinator p)) ERR-NOT-AUTHORIZED)
    (map-set programs program-id (merge p { status: "completed" })) (ok true)))

(define-read-only (get-program (id uint)) (map-get? programs id))
(define-read-only (get-kit (program-id uint) (index uint)) (map-get? supply-kits { program-id: program-id, index: index }))
(define-read-only (get-donor (program-id uint) (who principal)) (ok (default-to u0 (map-get? program-donors { program-id: program-id, donor: who }))))
(define-read-only (get-program-count) (ok (var-get program-count)))
(define-read-only (get-students-helped) (ok (var-get students-helped)))
