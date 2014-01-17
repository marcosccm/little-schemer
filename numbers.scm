(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define add
  (lambda (x y)
    (cond
      ((zero? y) x)
      (else (add1 (add x (sub1 y)))))))

(define sub
  (lambda (x y)
    (cond
      ((zero? y) x)
      (else (sub1 (sub x (sub1 y)))))))

(define mult
  (lambda (x y)
    (cond
      ((zero? y) 0)
      (else (add x (mult x (sub1 y)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (add (car tup) (addtup (cdr tup)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) '())
      (else (cons 
              (add (car tup1) (car tup2))
              (tup+ (cdr tup1) (cdr tup2)))))))

(define gt
  (lambda (x y)
    (cond
      ((zero? x) #f)
      ((zero? y) #t)
      (else (gt (sub1 x) (sub1 y))))))

(define lt
  (lambda (x y)
    (cond
      ((zero? y) #f)
      ((zero? x) #t)
      (else (lt (sub1 x) (sub1 y))))))

(define eq
  (lambda (x y)
    (cond
      ((lt x y) #f)
      ((gt x y) #f)
      (else #t))))

(define expo
  (lambda (x y)
    (cond
      ((zero? y) 1)
      (else (mult x (expo x (sub1 y)))))))

(define div
  (lambda (x y)
    (cond 
      ((lt x y) 0)
      (else (add1 (div (sub x y) y))))))

(define lenght
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (lenght (cdr l)))))))

(define pick
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (pick (sub1 n) (cdr l))))))

(define rempick
  (lambda (n l)
    (cond
      ((zero? n) (cdr l))
      (else (cons (car l) (rempick (sub1 n) (cdr l)))))))

(define no-nums
  (lambda (l)
    (cond
      ((null? l) '())
      ((number? (car l)) (cons (car l) (no-nums (cdr l))))
      (else (no-nums (cdr l))))))

(define occur
  (lambda (elem l)
    (cond
      ((null? l) 0)
      ((eq? (car l) elem) (add1 (occur elem (cdr l))))
      (else (occur elem (cdr l))))))
