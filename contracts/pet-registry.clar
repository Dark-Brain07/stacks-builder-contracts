;; Pet Registry Contract
;; Pet registration and ownership tracking
;; Halal - responsible pet care
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var pet-count uint u0)
(define-data-var transfer-count uint u0)

(define-map pets uint { owner: principal, name: (string-utf8 50), species: (string-ascii 20), breed: (string-utf8 50), age-months: uint, vaccinated: bool, microchip: (string-ascii 30), registered: uint })
(define-map owner-pets { owner: principal, index: uint } uint)
(define-map owner-pet-count principal uint)
(define-map transfers uint { pet-id: uint, from: principal, to: principal, block: uint })
(define-map vet-records { pet-id: uint, index: uint } { vet: (string-utf8 100), procedure: (string-utf8 100), block: uint })
(define-map vet-count uint uint)

(define-public (register-pet (name (string-utf8 50)) (species (string-ascii 20)) (breed (string-utf8 50)) (age uint) (microchip (string-ascii 30)))
  (let (
    (id (+ (var-get pet-count) u1))
    (oc (default-to u0 (map-get? owner-pet-count tx-sender)))
  )
    (map-set pets id { owner: tx-sender, name: name, species: species, breed: breed, age-months: age, vaccinated: false, microchip: microchip, registered: stacks-block-height })
    (map-set owner-pets { owner: tx-sender, index: oc } id)
    (map-set owner-pet-count tx-sender (+ oc u1))
    (var-set pet-count id) (ok id)))

(define-public (transfer-pet (pet-id uint) (new-owner principal))
  (let (
    (pet (unwrap! (map-get? pets pet-id) ERR-NOT-FOUND))
    (tid (+ (var-get transfer-count) u1))
  )
    (asserts! (is-eq tx-sender (get owner pet)) ERR-NOT-AUTHORIZED)
    (map-set pets pet-id (merge pet { owner: new-owner }))
    (map-set transfers tid { pet-id: pet-id, from: tx-sender, to: new-owner, block: stacks-block-height })
    (var-set transfer-count tid) (ok tid)))

(define-public (update-vaccination (pet-id uint) (status bool))
  (let ((pet (unwrap! (map-get? pets pet-id) ERR-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender (get owner pet)) (is-eq tx-sender CONTRACT-OWNER)) ERR-NOT-AUTHORIZED)
    (map-set pets pet-id (merge pet { vaccinated: status })) (ok true)))

(define-public (add-vet-record (pet-id uint) (vet (string-utf8 100)) (procedure (string-utf8 100)))
  (let (
    (pet (unwrap! (map-get? pets pet-id) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? vet-count pet-id)))
  )
    (asserts! (is-eq tx-sender (get owner pet)) ERR-NOT-AUTHORIZED)
    (map-set vet-records { pet-id: pet-id, index: idx } { vet: vet, procedure: procedure, block: stacks-block-height })
    (map-set vet-count pet-id (+ idx u1)) (ok idx)))

(define-read-only (get-pet (id uint)) (map-get? pets id))
(define-read-only (get-transfer (id uint)) (map-get? transfers id))
(define-read-only (get-vet-record (pet-id uint) (index uint)) (map-get? vet-records { pet-id: pet-id, index: index }))
(define-read-only (get-pet-count) (ok (var-get pet-count)))
(define-read-only (get-owner-pet-count (who principal)) (ok (default-to u0 (map-get? owner-pet-count who))))
