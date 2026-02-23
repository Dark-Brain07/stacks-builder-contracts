;; Compost Hub Contract
;; Community composting cooperative
;; Halal - environmental stewardship
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))

(define-data-var hub-count uint u0)
(define-data-var total-composted uint u0)

(define-map hubs uint { manager: principal, name: (string-utf8 100), location: (string-utf8 100), members: uint, total-kg: uint, compost-ready-kg: uint, active: bool })
(define-map hub-members { hub-id: uint, member: principal } { contributions-kg: uint, compost-received-kg: uint, joined: uint })
(define-map drop-offs { hub-id: uint, index: uint } { member: principal, kg: uint, material: (string-ascii 20), block: uint })
(define-map drop-count uint uint)

(define-public (create-hub (name (string-utf8 100)) (location (string-utf8 100)))
  (let ((id (+ (var-get hub-count) u1)))
    (map-set hubs id { manager: tx-sender, name: name, location: location, members: u0, total-kg: u0, compost-ready-kg: u0, active: true })
    (var-set hub-count id) (ok id)))

(define-public (join-hub (hub-id uint))
  (let ((hub (unwrap! (map-get? hubs hub-id) ERR-NOT-FOUND)))
    (asserts! (get active hub) ERR-NOT-FOUND)
    (map-set hub-members { hub-id: hub-id, member: tx-sender } { contributions-kg: u0, compost-received-kg: u0, joined: stacks-block-height })
    (map-set hubs hub-id (merge hub { members: (+ (get members hub) u1) })) (ok true)))

(define-public (drop-off-waste (hub-id uint) (kg uint) (material (string-ascii 20)))
  (let (
    (hub (unwrap! (map-get? hubs hub-id) ERR-NOT-FOUND))
    (m (unwrap! (map-get? hub-members { hub-id: hub-id, member: tx-sender }) ERR-NOT-FOUND))
    (idx (default-to u0 (map-get? drop-count hub-id)))
  )
    (map-set drop-offs { hub-id: hub-id, index: idx } { member: tx-sender, kg: kg, material: material, block: stacks-block-height })
    (map-set drop-count hub-id (+ idx u1))
    (map-set hub-members { hub-id: hub-id, member: tx-sender } (merge m { contributions-kg: (+ (get contributions-kg m) kg) }))
    (map-set hubs hub-id (merge hub { total-kg: (+ (get total-kg hub) kg) }))
    (var-set total-composted (+ (var-get total-composted) kg)) (ok kg)))

(define-public (mark-compost-ready (hub-id uint) (kg uint))
  (let ((hub (unwrap! (map-get? hubs hub-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get manager hub)) ERR-NOT-AUTHORIZED)
    (map-set hubs hub-id (merge hub { compost-ready-kg: (+ (get compost-ready-kg hub) kg) })) (ok kg)))

(define-public (collect-compost (hub-id uint) (kg uint))
  (let (
    (hub (unwrap! (map-get? hubs hub-id) ERR-NOT-FOUND))
    (m (unwrap! (map-get? hub-members { hub-id: hub-id, member: tx-sender }) ERR-NOT-FOUND))
  )
    (asserts! (>= (get compost-ready-kg hub) kg) ERR-NOT-FOUND)
    (map-set hubs hub-id (merge hub { compost-ready-kg: (- (get compost-ready-kg hub) kg) }))
    (map-set hub-members { hub-id: hub-id, member: tx-sender } (merge m { compost-received-kg: (+ (get compost-received-kg m) kg) }))
    (ok kg)))

(define-read-only (get-hub (id uint)) (map-get? hubs id))
(define-read-only (get-member (hub-id uint) (who principal)) (map-get? hub-members { hub-id: hub-id, member: who }))
(define-read-only (get-drop-off (hub-id uint) (index uint)) (map-get? drop-offs { hub-id: hub-id, index: index }))
(define-read-only (get-hub-count) (ok (var-get hub-count)))
(define-read-only (get-total-composted) (ok (var-get total-composted)))
