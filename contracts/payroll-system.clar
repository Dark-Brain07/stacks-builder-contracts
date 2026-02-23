;; Payroll System Contract
;; On-chain payroll distribution
;; Halal - fair compensation
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-PAID (err u405))

(define-data-var employee-count uint u0)
(define-data-var total-payouts uint u0)
(define-data-var pay-period-count uint u0)

(define-map employees principal { salary: uint, role: (string-utf8 50), active: bool, added-at: uint })
(define-map pay-records { employee: principal, period: uint } { amount: uint, paid-at: uint })

(define-public (add-employee (employee principal) (salary uint) (role (string-utf8 50)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set employees employee { salary: salary, role: role, active: true, added-at: stacks-block-height })
    (var-set employee-count (+ (var-get employee-count) u1)) (ok true)))

(define-public (update-salary (employee principal) (new-salary uint))
  (let ((emp (unwrap! (map-get? employees employee) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set employees employee (merge emp { salary: new-salary })) (ok true)))

(define-public (remove-employee (employee principal))
  (let ((emp (unwrap! (map-get? employees employee) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set employees employee (merge emp { active: false }))
    (var-set employee-count (- (var-get employee-count) u1)) (ok true)))

(define-public (pay-employee (employee principal))
  (let (
    (emp (unwrap! (map-get? employees employee) ERR-NOT-FOUND))
    (period (var-get pay-period-count))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (get active emp) ERR-NOT-FOUND)
    (asserts! (is-none (map-get? pay-records { employee: employee, period: period })) ERR-ALREADY-PAID)
    (try! (stx-transfer? (get salary emp) tx-sender employee))
    (map-set pay-records { employee: employee, period: period } { amount: (get salary emp), paid-at: stacks-block-height })
    (var-set total-payouts (+ (var-get total-payouts) (get salary emp)))
    (ok (get salary emp))))

(define-public (advance-period)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set pay-period-count (+ (var-get pay-period-count) u1)) (ok (var-get pay-period-count))))

(define-read-only (get-employee (who principal)) (map-get? employees who))
(define-read-only (get-pay-record (employee principal) (period uint)) (map-get? pay-records { employee: employee, period: period }))
(define-read-only (get-employee-count) (ok (var-get employee-count)))
(define-read-only (get-total-payouts) (ok (var-get total-payouts)))
(define-read-only (get-current-period) (ok (var-get pay-period-count)))
