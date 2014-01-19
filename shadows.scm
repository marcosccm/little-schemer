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
