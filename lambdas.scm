(load "numbers.scm")

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

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat)
                    ((multirember-f test?) a (cdr lat))))))))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else cons (car lat)
            (multiremberT test? (cdr lat))))))

(define multirember-co
  (lambda (a lat col)
    (cond
      ((null? lat) (col '() '()))
      ((eq? (car lat) a)
        (multirember-co a
          (cdr lat)
          (lambda (newlat seen)
            (col newlat (cons (car lat) seen)))))
      (else
        (multiremner-co a
          (cdr lat)
          (lambda (newlat seen)
            (col (cons (car lat) newlat) seen)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new (cons old (multiinsertL new old (cdr l)))))
      (else
        (cons (car l) (multiinsertL new old (cdr l)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL)
       (cons new (cons oldL (multiinsertLR oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR (cons new (multiinsertLR oldL oldR (cdr lat)))))
      (else
        (cons (car lat) (multiinsert oldL oldR (cdr lat)))))))

(define multiinsertLR-co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR-co new oldL oldR
        (cdr lat)
        (lambda (newlat l r)
          (col (cons new (cons oldL newlat)) (add1 l) r))))
      ((eq? (car lat) oldR)
       (multiinsertLR-co new oldL oldR
        (cdr lat)
        (lambda (newlat l r)
          (col (cons oldR (cons new newlat)) l (add1 r)))))
      (else
        (multiinsertLR-co new oldL oldR
          (cdr lat)
          (lambda (newllat l r)
            (cons (car lat) newlat) l r))))))

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

(define evens-only*-co
  (lambda (lat col)
    (cond
      ((null? lat)
       (col '() 1 0))
      ((atom? (car lat))
       (cond
         ((even? (car lat))
          (evens-only*-co (cdr lat)
            (lambda (newlat evens odd)
              (col (cons (car lat) newlat) (mult evens (car lat)) odd))))
          (else
            (evens-only*-co (cdr lat)
              (lambda (newlat evens odd)
                (col newlat evens (add (car lat) odd)))))))
      (else (evens-only*-co (car lat)
              (lambda (al ap as)
                (evens-only*-co (cdr lat)
                  (lambda (dl dp ds)
                    (col (cons al ap) (mult ap dp) (add as ds))))))))))

