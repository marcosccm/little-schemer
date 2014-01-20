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

(define eqset?
  (lambda (s1 s2)
    (and
      (subset? s1 s2)
      (subset? s2 s1))))

(define intersect?
  (lambda (s1 s2)
    (cond
      ((null? s1) #f)
      ((member? (car s1) s2) #t)
      (else (intersect? (cdr s1) s2)))))

(define intersect
  (lambda (s1 s2)
    (cond
      ((null? s1) '())
      ((member? (car s1) s2) (cons (car s1) (intersect (cdr s1) s2)))
      (else (intersect (cdr s1) s2)))))

(define union
  (lambda (s1 s2)
    (cond
      ((null? s1) s2)
      ((member? (car s1) s2) (union (cdr s1) s2))
      (else (cons (car s1) (union (cdr s1) s2))))))

(define intersectall
  (lambda (lset)
    (cond
      ((null? (car lset)) (cdr lset))
      (else (intersect (car lset) (intersectall (cdr lset)))))))
