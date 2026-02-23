;; Honey Coop Contract
;; Beekeeping cooperative
;; Halal - natural sustenance (honey in Quran)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var hive-count uint u0)
(define-data-var total-harvest-kg uint u0)
(define-data-var total-sales uint u0)

(define-map hives uint { keeper: principal, location: (string-utf8 100), colony-strength: (string-ascii 20), total-honey-kg: uint, active: bool, installed: uint })
(define-map harvests { hive-id: uint, index: uint } { kg: uint, quality: (string-ascii 20), block: uint })
(define-map harvest-count uint uint)
(define-map honey-sales { hive-id: uint, index: uint } { buyer: principal, kg: uint, price: uint, block: uint })
(define-map sale-count uint uint)

(define-public (register-hive (location (string-utf8 100)) (colony-strength (string-ascii 20)))
  (let ((id (+ (var-get hive-count) u1)))
    (map-set hives id { keeper: tx-sender, location: location, colony-strength: colony-strength, total-honey-kg: u0, active: true, installed: stacks-block-height })
    (var-set hive-count id) (ok id)))

(define-public (record-harvest (hive-id uint) (kg uint) (quality (string-ascii 20)))
  (let (
    (hive (unwrap! (map-get? hives hive-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? harvest-count hive-id)))
  )
    (asserts! (is-eq tx-sender (get keeper hive)) ERR-NOT-AUTHORIZED)
    (map-set harvests { hive-id: hive-id, index: idx } { kg: kg, quality: quality, block: stacks-block-height })
    (map-set harvest-count hive-id (+ idx u1))
    (map-set hives hive-id (merge hive { total-honey-kg: (+ (get total-honey-kg hive) kg) }))
    (var-set total-harvest-kg (+ (var-get total-harvest-kg) kg)) (ok kg)))

(define-public (sell-honey (hive-id uint) (buyer principal) (kg uint) (price uint))
  (let (
    (hive (unwrap! (map-get? hives hive-id) ERR-NOT-FOUND))
    (sidx (default-to u0 (map-get? sale-count hive-id)))
  )
    (asserts! (is-eq tx-sender (get keeper hive)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? price buyer tx-sender))
    (map-set honey-sales { hive-id: hive-id, index: sidx } { buyer: buyer, kg: kg, price: price, block: stacks-block-height })
    (map-set sale-count hive-id (+ sidx u1))
    (var-set total-sales (+ (var-get total-sales) price)) (ok price)))

(define-public (update-colony (hive-id uint) (strength (string-ascii 20)))
  (let ((hive (unwrap! (map-get? hives hive-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get keeper hive)) ERR-NOT-AUTHORIZED)
    (map-set hives hive-id (merge hive { colony-strength: strength })) (ok true)))

(define-read-only (get-hive (id uint)) (map-get? hives id))
(define-read-only (get-harvest (hive-id uint) (index uint)) (map-get? harvests { hive-id: hive-id, index: index }))
(define-read-only (get-sale (hive-id uint) (index uint)) (map-get? honey-sales { hive-id: hive-id, index: index }))
(define-read-only (get-hive-count) (ok (var-get hive-count)))
(define-read-only (get-total-harvest) (ok (var-get total-harvest-kg)))
