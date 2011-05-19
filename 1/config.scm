(use util.list)
(use gauche.collection)

(define (alist-tree->hash-table tree)
  (define hash (make-hash-table))
  (for-each
    (lambda (e) (let ((name (car e)) (it (cdr e)))
      (hash-table-put! hash name (cond 
         ((pair? it) (alist-tree->hash-table it))
         ((vector? it) (vector->list it))
         (#t it)))))
    tree)
  hash)

(define config (alist-tree->hash-table '(
  (keys
    (quit . #\q)
    (restart . #\r)))))
