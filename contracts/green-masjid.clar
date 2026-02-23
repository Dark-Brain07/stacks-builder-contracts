;; Green Masjid Contract
;; Eco-friendly mosque initiative
;; Halal - environmental stewardship in masjid
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var masjid-count uint u0)
(define-data-var total-invested uint u0)

(define-map masjids uint { imam: principal, name: (string-utf8 100), location: (string-utf8 100), energy-score: uint, projects: uint, total-investment: uint })
(define-map green-projects { masjid-id: uint, index: uint } { name: (string-utf8 100), category: (string-ascii 20), cost: uint, savings-per-year: uint, status: (string-ascii 20), started: uint })
(define-map project-count uint uint)
(define-map green-donors { masjid-id: uint, donor: principal } uint)

(define-public (register-masjid (name (string-utf8 100)) (location (string-utf8 100)))
  (let ((id (+ (var-get masjid-count) u1)))
    (map-set masjids id { imam: tx-sender, name: name, location: location, energy-score: u0, projects: u0, total-investment: u0 })
    (var-set masjid-count id) (ok id)))

(define-public (propose-project (masjid-id uint) (name (string-utf8 100)) (category (string-ascii 20)) (cost uint) (savings uint))
  (let (
    (m (unwrap! (map-get? masjids masjid-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? project-count masjid-id)))
  )
    (asserts! (is-eq tx-sender (get imam m)) ERR-NOT-AUTHORIZED)
    (map-set green-projects { masjid-id: masjid-id, index: idx } { name: name, category: category, cost: cost, savings-per-year: savings, status: "proposed", started: stacks-block-height })
    (map-set project-count masjid-id (+ idx u1))
    (map-set masjids masjid-id (merge m { projects: (+ (get projects m) u1) })) (ok idx)))

(define-public (fund-project (masjid-id uint) (amount uint))
  (let (
    (m (unwrap! (map-get? masjids masjid-id) ERR-NOT-FOUND))
    (prev (default-to u0 (map-get? green-donors { masjid-id: masjid-id, donor: tx-sender })))
  )
    (try! (stx-transfer? amount tx-sender (get imam m)))
    (map-set green-donors { masjid-id: masjid-id, donor: tx-sender } (+ prev amount))
    (map-set masjids masjid-id (merge m { total-investment: (+ (get total-investment m) amount) }))
    (var-set total-invested (+ (var-get total-invested) amount)) (ok amount)))

(define-public (complete-project (masjid-id uint) (project-index uint))
  (let (
    (m (unwrap! (map-get? masjids masjid-id) ERR-NOT-FOUND))
    (p (unwrap! (map-get? green-projects { masjid-id: masjid-id, index: project-index }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get imam m)) ERR-NOT-AUTHORIZED)
    (map-set green-projects { masjid-id: masjid-id, index: project-index } (merge p { status: "completed" })) (ok true)))

(define-public (update-energy-score (masjid-id uint) (score uint))
  (let ((m (unwrap! (map-get? masjids masjid-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get imam m)) ERR-NOT-AUTHORIZED)
    (map-set masjids masjid-id (merge m { energy-score: score })) (ok score)))

(define-read-only (get-masjid (id uint)) (map-get? masjids id))
(define-read-only (get-project (masjid-id uint) (index uint)) (map-get? green-projects { masjid-id: masjid-id, index: index }))
(define-read-only (get-donor (masjid-id uint) (who principal)) (ok (default-to u0 (map-get? green-donors { masjid-id: masjid-id, donor: who }))))
(define-read-only (get-masjid-count) (ok (var-get masjid-count)))
(define-read-only (get-total-invested) (ok (var-get total-invested)))
