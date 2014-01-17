(define member?
  (lambda (elem l)
    (cond
      ((null? l) #f)
      (else (or (eq? (car l) elem)
                (member? elem (cdr l)))))))

(define rember
  (lambda (elem l)
    (cond
      ((null? l) '())
      ((eql? (car l) elem) (cdr l))
      (else cons (car l) 
        (rember elem (cdr l))))))

(define firsts
  (lambda (l)
    (cond 
      ((null? l) '())
      (else (cons 
              (car (car l)) 
              (firsts (cdr l)))))))

(define insertR
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? (car l) old) 
        (cons 
          old 
          (cons new (cdr l))))
      (else (cons 
          (car l) 
          (insertR new old (cdr l)))))))

(define insertL
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? (car l) old) (cons new l))
      (else (cons (car l) (insertL new old (cdr l)))))))

(define subst
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? (car l) old) (cons new (cdr l)))
      (else (cons (car l) (subst new old (cdr l)))))))
