;; Task Manager Contract
;; On-chain project/task management
;; Halal - productivity and organization
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-DONE (err u405))

(define-data-var task-count uint u0)
(define-data-var completed-count uint u0)

(define-map tasks uint {
  creator: principal, assignee: principal, title: (string-utf8 100),
  priority: uint, status: (string-ascii 20), created: uint, completed-at: uint
})
(define-map user-task-count principal uint)
(define-map user-completed principal uint)

(define-public (create-task (title (string-utf8 100)) (assignee principal) (priority uint))
  (let ((id (+ (var-get task-count) u1)))
    (map-set tasks id {
      creator: tx-sender, assignee: assignee, title: title,
      priority: priority, status: "todo", created: stacks-block-height, completed-at: u0
    })
    (map-set user-task-count assignee (+ (default-to u0 (map-get? user-task-count assignee)) u1))
    (var-set task-count id) (ok id)))

(define-public (update-status (task-id uint) (new-status (string-ascii 20)))
  (let ((task (unwrap! (map-get? tasks task-id) ERR-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender (get assignee task)) (is-eq tx-sender (get creator task))) ERR-NOT-AUTHORIZED)
    (map-set tasks task-id (merge task { status: new-status })) (ok true)))

(define-public (complete-task (task-id uint))
  (let ((task (unwrap! (map-get? tasks task-id) ERR-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender (get assignee task)) (is-eq tx-sender (get creator task))) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-eq (get status task) "done")) ERR-ALREADY-DONE)
    (map-set tasks task-id (merge task { status: "done", completed-at: stacks-block-height }))
    (map-set user-completed (get assignee task) (+ (default-to u0 (map-get? user-completed (get assignee task))) u1))
    (var-set completed-count (+ (var-get completed-count) u1)) (ok true)))

(define-public (reassign-task (task-id uint) (new-assignee principal))
  (let ((task (unwrap! (map-get? tasks task-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get creator task)) ERR-NOT-AUTHORIZED)
    (map-set tasks task-id (merge task { assignee: new-assignee })) (ok true)))

(define-read-only (get-task (id uint)) (map-get? tasks id))
(define-read-only (get-task-count) (ok (var-get task-count)))
(define-read-only (get-completed-count) (ok (var-get completed-count)))
(define-read-only (get-user-tasks (who principal)) (ok (default-to u0 (map-get? user-task-count who))))
(define-read-only (get-user-completed (who principal)) (ok (default-to u0 (map-get? user-completed who))))
