(load "list.scm")

(define set?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((member? (car l) (cdr l)) #f)
      (else (set? (cdr l))))))

(define makeset
  (lambda (l)
    (cond
      ((null? l) '())
      ((member? (car l) (cdr l)) (makeset (cdr l)))
      (else (cons (car l) (makeset (cdr l)))))))

(define subset?
  (lambda (s1 s2)
    (cond
      ((null? s1) #t)
      ((member? (car s1) s2) (subset? (cdr s1) s1))
      (else #f))))
