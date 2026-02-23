;; Book Lending Contract
;; Community book lending library
;; Halal - spreading knowledge
;; Clarity 4 compatible

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-UNAVAILABLE (err u405))

(define-data-var book-count uint u0)
(define-data-var loan-count uint u0)

(define-map books uint { owner: principal, title: (string-utf8 100), author: (string-utf8 100), isbn: (string-ascii 20), available: bool, times-lent: uint })
(define-map loans uint { book-id: uint, borrower: principal, borrowed-at: uint, due-block: uint, returned: bool })
(define-map borrower-stats principal { books-borrowed: uint, books-returned: uint })

(define-public (add-book (title (string-utf8 100)) (author (string-utf8 100)) (isbn (string-ascii 20)))
  (let ((id (+ (var-get book-count) u1)))
    (map-set books id { owner: tx-sender, title: title, author: author, isbn: isbn, available: true, times-lent: u0 })
    (var-set book-count id) (ok id)))

(define-public (borrow-book (book-id uint) (duration uint))
  (let (
    (book (unwrap! (map-get? books book-id) ERR-NOT-FOUND))
    (lid (+ (var-get loan-count) u1))
    (stats (default-to { books-borrowed: u0, books-returned: u0 } (map-get? borrower-stats tx-sender)))
  )
    (asserts! (get available book) ERR-UNAVAILABLE)
    (map-set loans lid { book-id: book-id, borrower: tx-sender, borrowed-at: stacks-block-height, due-block: (+ stacks-block-height duration), returned: false })
    (map-set books book-id (merge book { available: false, times-lent: (+ (get times-lent book) u1) }))
    (map-set borrower-stats tx-sender (merge stats { books-borrowed: (+ (get books-borrowed stats) u1) }))
    (var-set loan-count lid) (ok lid)))

(define-public (return-book (loan-id uint))
  (let (
    (loan (unwrap! (map-get? loans loan-id) ERR-NOT-FOUND))
    (book (unwrap! (map-get? books (get book-id loan)) ERR-NOT-FOUND))
    (stats (default-to { books-borrowed: u0, books-returned: u0 } (map-get? borrower-stats tx-sender)))
  )
    (asserts! (is-eq tx-sender (get borrower loan)) ERR-NOT-AUTHORIZED)
    (map-set loans loan-id (merge loan { returned: true }))
    (map-set books (get book-id loan) (merge book { available: true }))
    (map-set borrower-stats tx-sender (merge stats { books-returned: (+ (get books-returned stats) u1) }))
    (ok true)))

(define-public (remove-book (book-id uint))
  (let ((book (unwrap! (map-get? books book-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner book)) ERR-NOT-AUTHORIZED)
    (asserts! (get available book) ERR-UNAVAILABLE)
    (map-set books book-id (merge book { available: false })) (ok true)))

(define-read-only (get-book (id uint)) (map-get? books id))
(define-read-only (get-loan (id uint)) (map-get? loans id))
(define-read-only (get-borrower-stats (who principal)) (map-get? borrower-stats who))
(define-read-only (get-book-count) (ok (var-get book-count)))
(define-read-only (get-loan-count) (ok (var-get loan-count)))
