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
      ((eq? (operator aexp) '+)
       (add
         (value2 (car (cdr aexp)))
         (value2 (car (cdr (cdr aexp))))))
      ((eq? (operator aexp) 'x)
       (add
         (value2 (car (cdr aexp)))
         (value2 (car (cdr (cdr aexp))))))
      (else 
       (expo
         (value2 (fst-sub-exp aexp))
         (value2 (snd-sub-exp aexp)))))))

(define fst-sub-exp
  (lambda (aexp)
    (car (cdr (aexp)))))

(define snd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr (aexp))))))

(define operator
  (lambda (aexp)
    (car aexp)))
