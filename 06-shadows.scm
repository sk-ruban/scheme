(define atom?                                                                
 (lambda (x)                                                                 
    (and (not (pair? x)) (not (null? x))))) 

; an arithmetic expression is either an atom or 2 expressions combined by operators, no parentheses
; `quote` creates a string
; `numbered` checks if a representation of an arithmetic expression only contains numbers and operators

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) 'o+)
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) 'ox)
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) 'o^)
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      (else #f))))

(define numbered2?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
        (and (numbered2? (car aexp))
             (numbered2? (car (cdr (cdr aexp)))))))))

(numbered2? '5)
(numbered2? '(cookie))
(numbered2? '(5 + 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The seventh commandment                                                    ;
;                                                                            ;
; Recur on the subparts that are of the same nature:                         ;
; * On the sublists of a list.                                               ;
; * On the subexpressions of an arithmetic expression.                       ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; `value nexp` returns the natural value of an arithmetic expression (+ 5 3)
; we can change 1st-sub-exp and 2nd-sub-exp for different orders of arithmetic operations

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+)
       (+ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
      ((eq? (car nexp) '*)
       (* (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
      ((eq? (car nexp) '^)
       (expt (value (1st-sub-exp nexp))
             (value (2nd-sub-exp nexp))))
      (else #f))))

(value '(+ 3 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The eighth commandment                                                     ;
;                                                                            ;
; Use help functions to abstract from representations.                       ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A different number representation:
; () for zero, (()) for one, (() ()) for two, (() () ()) for three, etc.

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(zub1 '(() () ()))
(edd1 '(() () ()))

(define .+
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (.+ n (zub1 m)))))))

(.+ '(() () ()) '(() ()))