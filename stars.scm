(define rember*
  (lambda (elem l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? elem (car l)) (rember* elem (cdr l)))
          (else (cons 
                  (car l) 
                  (rember* elem (cdr l))))))
      (else (cons 
              (rember* elem (car l))
              (rember* elem (cdr l)))))))

(define occur*
  (lambda (elem l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
        (cond
          ((eq? (car l) elem)
          (add1 (occur* elem (cdr l))))
          (else (occur* elem (cdr l)))))
      (else (add 
              (occur* elem (car l))
              (occur* elem (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
        ((eq? (car l) old) (cons new 
                              (cons old 
                                (insertL* new old (cdr l)))))
        (else (cons 
                (car l) 
                (insertL* new old (cdr l))))))
      (else (cons 
              (insertL* new old (car l))
              (insertL* new old (cdr l)))))))

(define member*
  (lambda (elem l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or
         (eq? (car l) elem)
         (member* elem (cdr l))))
      (else (or
              (member* elem (car l))
              (member* elem (cdr l)))))))

(define leftmost*
  (lambda (l)
    (cond
      ((atom? (car l) (car l)))
      (else (leftmost* (car l))))))
