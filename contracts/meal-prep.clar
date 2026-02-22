;; Meal Prep Contract
;; Community meal preparation and sharing
;; Halal - feeding community
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-FULL (err u405))

(define-data-var meal-count uint u0)
(define-data-var order-count uint u0)
(define-data-var total-meals-served uint u0)

(define-map meals uint { chef: principal, name: (string-utf8 100), description: (string-utf8 200), servings: uint, claimed: uint, price: uint, halal-verified: bool, posted: uint })
(define-map orders uint { meal-id: uint, customer: principal, servings: uint, paid: uint, status: (string-ascii 20), block: uint })
(define-map chef-profiles principal { name: (string-utf8 100), meals-cooked: uint, total-earned: uint, rating-total: uint, ratings: uint })

(define-public (register-chef (name (string-utf8 100)))
  (begin (map-set chef-profiles tx-sender { name: name, meals-cooked: u0, total-earned: u0, rating-total: u0, ratings: u0 }) (ok true)))

(define-public (post-meal (name (string-utf8 100)) (description (string-utf8 200)) (servings uint) (price uint))
  (let ((id (+ (var-get meal-count) u1)))
    (asserts! (is-some (map-get? chef-profiles tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set meals id { chef: tx-sender, name: name, description: description, servings: servings, claimed: u0, price: price, halal-verified: false, posted: stacks-block-height })
    (var-set meal-count id) (ok id)))

(define-public (verify-halal (meal-id uint))
  (let ((meal (unwrap! (map-get? meals meal-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set meals meal-id (merge meal { halal-verified: true })) (ok true)))

(define-public (order-meal (meal-id uint) (servings uint))
  (let (
    (meal (unwrap! (map-get? meals meal-id) ERR-NOT-FOUND))
    (cost (* servings (get price meal)))
    (oid (+ (var-get order-count) u1))
    (chef-profile (default-to { name: u"", meals-cooked: u0, total-earned: u0, rating-total: u0, ratings: u0 } (map-get? chef-profiles (get chef meal))))
  )
    (asserts! (>= (- (get servings meal) (get claimed meal)) servings) ERR-FULL)
    (try! (stx-transfer? cost tx-sender (get chef meal)))
    (map-set orders oid { meal-id: meal-id, customer: tx-sender, servings: servings, paid: cost, status: "ordered", block: stacks-block-height })
    (map-set meals meal-id (merge meal { claimed: (+ (get claimed meal) servings) }))
    (map-set chef-profiles (get chef meal) (merge chef-profile { meals-cooked: (+ (get meals-cooked chef-profile) u1), total-earned: (+ (get total-earned chef-profile) cost) }))
    (var-set order-count oid)
    (var-set total-meals-served (+ (var-get total-meals-served) servings))
    (ok oid)))

(define-public (rate-meal (order-id uint) (rating uint))
  (let (
    (o (unwrap! (map-get? orders order-id) ERR-NOT-FOUND))
    (meal (unwrap! (map-get? meals (get meal-id o)) ERR-NOT-FOUND))
    (chef (default-to { name: u"", meals-cooked: u0, total-earned: u0, rating-total: u0, ratings: u0 } (map-get? chef-profiles (get chef meal))))
  )
    (asserts! (is-eq tx-sender (get customer o)) ERR-NOT-AUTHORIZED)
    (map-set chef-profiles (get chef meal) (merge chef { rating-total: (+ (get rating-total chef) rating), ratings: (+ (get ratings chef) u1) }))
    (ok rating)))

(define-read-only (get-meal (id uint)) (map-get? meals id))
(define-read-only (get-order (id uint)) (map-get? orders id))
(define-read-only (get-chef (who principal)) (map-get? chef-profiles who))
(define-read-only (get-meal-count) (ok (var-get meal-count)))
(define-read-only (get-total-served) (ok (var-get total-meals-served)))
