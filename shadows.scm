(load "atom.scm")
(load "numbers.scm")

(define value
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (car (cdr aexp)) '+)
       (add
         (value (car aexp))
         (value (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) 'x)
       (mult
         (value (car aexp))
         (value (car (cdr (cdr aexp))))))
      (else 
        (expo
          (value (car aexp))
          (value (car (cdr (cdr aexp)))))))))

(define value2
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (car aexp) '+)
       (add
         (value2 (car (cdr aexp)))
         (value2 (car (cdr (cdr aexp))))))
      ((eq? (car aexp) 'x)
       (add
         (value2 (car (cdr aexp)))
         (value2 (car (cdr (cdr aexp))))))
      (else 
       (expo
         (value2 (car (cdr aexp)))
         (value2 (car (cdr (cdr aexp)))))))))
