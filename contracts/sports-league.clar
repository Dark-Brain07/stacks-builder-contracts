;; Sports League Contract
;; Sports league and tournament management
;; Halal - healthy recreation
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-REGISTERED (err u405))

(define-data-var team-count uint u0)
(define-data-var match-count uint u0)
(define-data-var season-count uint u0)

(define-map teams uint { name: (string-utf8 100), captain: principal, wins: uint, losses: uint, draws: uint, points: uint, active: bool })
(define-map matches uint { home: uint, away: uint, home-score: uint, away-score: uint, season: uint, played: bool, block: uint })
(define-map seasons uint { name: (string-utf8 50), teams: uint, matches-played: uint, status: (string-ascii 20) })
(define-map team-names (string-utf8 100) uint)

(define-public (create-season (name (string-utf8 50)))
  (let ((id (+ (var-get season-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set seasons id { name: name, teams: u0, matches-played: u0, status: "active" })
    (var-set season-count id) (ok id)))

(define-public (register-team (name (string-utf8 100)))
  (let ((id (+ (var-get team-count) u1)))
    (asserts! (is-none (map-get? team-names name)) ERR-ALREADY-REGISTERED)
    (map-set teams id { name: name, captain: tx-sender, wins: u0, losses: u0, draws: u0, points: u0, active: true })
    (map-set team-names name id)
    (var-set team-count id) (ok id)))

(define-public (schedule-match (home-id uint) (away-id uint) (season-id uint))
  (let ((mid (+ (var-get match-count) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? teams home-id)) ERR-NOT-FOUND)
    (asserts! (is-some (map-get? teams away-id)) ERR-NOT-FOUND)
    (map-set matches mid { home: home-id, away: away-id, home-score: u0, away-score: u0, season: season-id, played: false, block: u0 })
    (var-set match-count mid) (ok mid)))

(define-public (record-result (match-id uint) (home-score uint) (away-score uint))
  (let (
    (m (unwrap! (map-get? matches match-id) ERR-NOT-FOUND))
    (home-team (unwrap! (map-get? teams (get home m)) ERR-NOT-FOUND))
    (away-team (unwrap! (map-get? teams (get away m)) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set matches match-id (merge m { home-score: home-score, away-score: away-score, played: true, block: stacks-block-height }))
    (if (> home-score away-score)
      (begin (map-set teams (get home m) (merge home-team { wins: (+ (get wins home-team) u1), points: (+ (get points home-team) u3) }))
        (map-set teams (get away m) (merge away-team { losses: (+ (get losses away-team) u1) })))
      (if (> away-score home-score)
        (begin (map-set teams (get away m) (merge away-team { wins: (+ (get wins away-team) u1), points: (+ (get points away-team) u3) }))
          (map-set teams (get home m) (merge home-team { losses: (+ (get losses home-team) u1) })))
        (begin (map-set teams (get home m) (merge home-team { draws: (+ (get draws home-team) u1), points: (+ (get points home-team) u1) }))
          (map-set teams (get away m) (merge away-team { draws: (+ (get draws away-team) u1), points: (+ (get points away-team) u1) })))))
    (ok true)))

(define-read-only (get-team (id uint)) (map-get? teams id))
(define-read-only (get-match (id uint)) (map-get? matches id))
(define-read-only (get-season (id uint)) (map-get? seasons id))
(define-read-only (get-team-count) (ok (var-get team-count)))
(define-read-only (get-match-count) (ok (var-get match-count)))
