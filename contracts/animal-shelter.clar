;; Animal Shelter Contract
;; Animal rescue, care and adoption management
;; Halal - compassion to animals (rahma)
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-ADOPTED (err u405))

(define-data-var animal-count uint u0)
(define-data-var adoption-count uint u0)
(define-data-var total-donations uint u0)

(define-map animals uint {
  name: (string-utf8 50), species: (string-ascii 20), age-months: uint,
  health: (string-ascii 20), adopter: (optional principal),
  care-cost: uint, status: (string-ascii 20), admitted: uint
})
(define-map adopters principal { adoptions: uint, total-donated: uint })
(define-map care-log { animal-id: uint, index: uint } { caretaker: principal, notes: (string-utf8 200), block: uint })
(define-map care-count uint uint)

(define-public (admit-animal (name (string-utf8 50)) (species (string-ascii 20)) (age uint) (health (string-ascii 20)) (care-cost uint))
  (let ((id (+ (var-get animal-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set animals id { name: name, species: species, age-months: age, health: health, adopter: none, care-cost: care-cost, status: "available", admitted: stacks-block-height })
    (var-set animal-count id) (ok id)))

(define-public (donate-care (animal-id uint) (amount uint))
  (let (
    (animal (unwrap! (map-get? animals animal-id) ERR-NOT-FOUND))
    (prev (default-to { adoptions: u0, total-donated: u0 } (map-get? adopters tx-sender)))
  )
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (map-set adopters tx-sender (merge prev { total-donated: (+ (get total-donated prev) amount) }))
    (var-set total-donations (+ (var-get total-donations) amount))
    (ok amount)))

(define-public (adopt-animal (animal-id uint))
  (let (
    (animal (unwrap! (map-get? animals animal-id) ERR-NOT-FOUND))
    (prev (default-to { adoptions: u0, total-donated: u0 } (map-get? adopters tx-sender)))
  )
    (asserts! (is-none (get adopter animal)) ERR-ALREADY-ADOPTED)
    (try! (stx-transfer? (get care-cost animal) tx-sender CONTRACT-OWNER))
    (map-set animals animal-id (merge animal { adopter: (some tx-sender), status: "adopted" }))
    (map-set adopters tx-sender (merge prev { adoptions: (+ (get adoptions prev) u1) }))
    (var-set adoption-count (+ (var-get adoption-count) u1))
    (ok true)))

(define-public (log-care (animal-id uint) (notes (string-utf8 200)))
  (let (
    (idx (default-to u0 (map-get? care-count animal-id)))
  )
    (asserts! (is-some (map-get? animals animal-id)) ERR-NOT-FOUND)
    (map-set care-log { animal-id: animal-id, index: idx } { caretaker: tx-sender, notes: notes, block: stacks-block-height })
    (map-set care-count animal-id (+ idx u1)) (ok idx)))

(define-read-only (get-animal (id uint)) (map-get? animals id))
(define-read-only (get-adopter (who principal)) (map-get? adopters who))
(define-read-only (get-care (animal-id uint) (index uint)) (map-get? care-log { animal-id: animal-id, index: index }))
(define-read-only (get-animal-count) (ok (var-get animal-count)))
(define-read-only (get-adoption-count) (ok (var-get adoption-count)))
(define-read-only (get-total-donations) (ok (var-get total-donations)))
