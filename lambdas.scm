(define eq?-c
  (lambda (x)
    (lambda (y)
      (eq? x y))))

(define eq-salad? (eq?-c 'salad))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? (car l) a) (cdr l))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define rember-eq (rember-f eq?))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old)
          (seq new old (cdr l)))
        (else
          (cons
            (car l)
            ((insert-g seq) new old (cdr l))))))))

(define insertL
  (insert-g
    (lambda (new old l)
      (cons new (cons old l)))))

(define insertR
  (insert-g
    (lambda (new old l)
      (cons old (cons new l)))))

(define subst
  (insert-g
    (lambda (new old l)
      (cons new l))))

(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(define seqrem
  (lambda (new old l)
    l))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x '+) add)
      ((eq? x 'x) mult)
      (else expo))))

(define value
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      (else
        ((atom-to-function (operator aexp))
          (value (fst-sub-exp aexp))
          (value (snd-sub-exp aexp)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

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
