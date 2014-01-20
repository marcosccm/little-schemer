
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old) (seq new old l))
        (else (cons (car l)
                ((insert-g seq) new old (cdr l))))))))

(define insertL
  (insert-g
    (lambda (new old l)
      (cons new (cons old l)))))

(define insertR
  (insert-g
    (lambda (new old l)
      (cons old (cons new l)))))

(define even?
  (lambda (x)
    (eq (mult (div x 2) 2) x)))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l) )
       (cond
         ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
         (else evens-only* (cdr l))))
      (else (cons (evens-only* (car l))
                  (evens-only* (cdr l)))))))
