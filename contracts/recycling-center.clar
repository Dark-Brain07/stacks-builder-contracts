;; Recycling Center Contract
;; Recycling center drop-off management
;; Halal - environmental responsibility
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var center-count uint u0)
(define-data-var dropoff-count uint u0)
(define-data-var total-recycled-kg uint u0)

(define-map centers uint { manager: principal, name: (string-utf8 100), location: (string-utf8 100), materials-accepted: (string-utf8 200), total-kg: uint, active: bool })
(define-map dropoffs uint { center-id: uint, recycler: principal, material: (string-ascii 20), kg: uint, reward-paid: uint, block: uint })
(define-map recycler-profiles principal { total-kg: uint, dropoffs-made: uint, rewards-earned: uint })
(define-map reward-rates (string-ascii 20) uint)

(map-set reward-rates "plastic" u5)
(map-set reward-rates "paper" u3)
(map-set reward-rates "metal" u10)
(map-set reward-rates "glass" u7)
(map-set reward-rates "organic" u2)

(define-public (register-center (name (string-utf8 100)) (location (string-utf8 100)) (materials (string-utf8 200)))
  (let ((id (+ (var-get center-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set centers id { manager: tx-sender, name: name, location: location, materials-accepted: materials, total-kg: u0, active: true })
    (var-set center-count id) (ok id)))

(define-public (add-center (manager principal) (name (string-utf8 100)) (location (string-utf8 100)) (materials (string-utf8 200)))
  (let ((id (+ (var-get center-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set centers id { manager: manager, name: name, location: location, materials-accepted: materials, total-kg: u0, active: true })
    (var-set center-count id) (ok id)))

(define-public (drop-off (center-id uint) (material (string-ascii 20)) (kg uint))
  (let (
    (center (unwrap! (map-get? centers center-id) ERR-NOT-FOUND))
    (rate (default-to u2 (map-get? reward-rates material)))
    (reward (* kg rate))
    (did (+ (var-get dropoff-count) u1))
    (profile (default-to { total-kg: u0, dropoffs-made: u0, rewards-earned: u0 } (map-get? recycler-profiles tx-sender)))
  )
    (asserts! (get active center) ERR-NOT-FOUND)
    (try! (stx-transfer? reward CONTRACT-OWNER tx-sender))
    (map-set dropoffs did { center-id: center-id, recycler: tx-sender, material: material, kg: kg, reward-paid: reward, block: stacks-block-height })
    (map-set recycler-profiles tx-sender (merge profile { total-kg: (+ (get total-kg profile) kg), dropoffs-made: (+ (get dropoffs-made profile) u1), rewards-earned: (+ (get rewards-earned profile) reward) }))
    (map-set centers center-id (merge center { total-kg: (+ (get total-kg center) kg) }))
    (var-set dropoff-count did)
    (var-set total-recycled-kg (+ (var-get total-recycled-kg) kg)) (ok reward)))

(define-public (update-reward-rate (material (string-ascii 20)) (rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set reward-rates material rate) (ok true)))

(define-read-only (get-center (id uint)) (map-get? centers id))
(define-read-only (get-dropoff (id uint)) (map-get? dropoffs id))
(define-read-only (get-recycler (who principal)) (map-get? recycler-profiles who))
(define-read-only (get-reward-rate (material (string-ascii 20))) (ok (default-to u0 (map-get? reward-rates material))))
(define-read-only (get-center-count) (ok (var-get center-count)))
(define-read-only (get-total-recycled) (ok (var-get total-recycled-kg)))
