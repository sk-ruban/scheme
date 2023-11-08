(define atom?
 (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? (car l) a) (cdr l))
      (else
        (cons (car l) (rember-f test? a (cdr l)))))))

(rember-f eq? 2 '(1 2 3 4 5))

; currying translates a function from callable as f(a, b, c) into callable as f(a)(b)(c)

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? a x))))

((eq?-c 'tuna) 'tuna)
((eq?-c 'tuna) 'salad)

(define eq?-salad (eq?-c 'salad))

(eq?-salad 'salad) 
(eq?-salad 'tuna)

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? (car l) a) (cdr l))
        (else
          (cons (car l) ((rember-f test?) a (cdr l))))))))

((rember-f eq?) 2 '(1 2 3 4 5))

(define rember-eq? (rember-f eq?))

(rember-eq? 'tuna '(tuna salad is good))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old) (cons new (cons old (cdr l))))
        (else
          (cons (car l) ((insertL-f test?) new old (cdr l))))))))

((insertL-f eq?) 'd 'e '(a b c e f g h))  

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old) (cons old (cons new (cdr l))))
        (else
          (cons (car l) ((insertR-f test?) new old (cdr l))))))))

((insertR-f eq?) 'd 'e '(a b c e f g h)) 

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old)
         (seq new old (cdr l)))
        (else
          (cons (car l) ((insert-g seq) new old (cdr l))))))))

(define insertL (insert-g seqL))

(define insertR (insert-g seqR))

(insertL 'd 'e '(a b c e f g d h))
(insertR 'd 'e '(a b c e f g d h))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst (insert-g seqS))

(subst 'topping 'fudge '(ice cream with fudge for dessert))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The ninth commandment                                                      ;
;                                                                            ;
; Abstract common patterns with a new function.                              ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; updating the value function from chapter 6
; atom-to-fuction converts the atom operators to functions

(define atom-to-function
  (lambda (atom)
    (cond
      ((eq? atom '+) +)
      ((eq? atom '*) *)
      ((eq? atom '^) expt)
      (else #f))))

(define operator
  (lambda (aexp)
    (car aexp)))

(atom-to-function (operator '(+ 5 3)))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
        ((atom-to-function (operator nexp))
         (value (1st-sub-exp nexp))
         (value (2nd-sub-exp nexp)))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(value 13)
(value '(+ 1 3)) 

; updating the multirember function from chapter 3 with currying

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a)
         ((multirember-f test?) a (cdr lat)))
        (else
          (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat))
       (multiremberT test? (cdr lat)))
      (else
        (cons (car lat)
              (multiremberT test? (cdr lat)))))))

(define eq?-tuna (eq?-c 'tuna))

(multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))

; col stands for collector

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col '() '()))
      ((eq? (car lat) a)
       (multirember&co a (cdr lat)
       (lambda (newlat seen)
         (col newlat (cons (car lat) seen)))))
      (else
        (multirember&co a (cdr lat)
                          (lambda (newlat seen)
                            (col (cons (car lat) newlat) seen)))))))

; `a-friend` checks if the second argument is an empty list

(define a-friend
  (lambda (x y)
    (null? y)))

; `multirember&co a lat f` looks at every atoms of the lat and sees whether it is eq? to a
; atoms which the answer is true is collected to list ls2 and false to ls1
; finally it determines the value of (f ls1 ls2)

(multirember&co 'tuna '() a-friend)
(multirember&co 'tuna '(tuna) a-friend)

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat (cons 'tuna seen))))

(multirember&co 'tuna '(strawberries tuna and swordfish) new-friend)

;`last-friend` checks the length of ls1

(define last-friend
  (lambda (x y)
    (length x)))

(multirember&co 'tuna '(strawberries tuna and swordfish) last-friend)
(multirember&co 'tuna '() last-friend)
(multirember&co 'tuna '(tuna) last-friend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The tenth commandment                                                      ;
;                                                                            ;
; Build functions to collect more than one value at a time.                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; `multiinsertLR` inserts to the left and to the right of oldL and oldR respectively

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL)
       (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else
        (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(multiinsertLR 'x 'a 'b '(a o a o b o b b a b o))

; `multiinsertLR&co` has a collector function

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new (cons oldL newlat))
                                (+ 1 L) R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR (cons new newlat))
                                L (+ 1 R)))))
      (else
        (multiinsertLR&co new oldL oldR (cdr lat)
                          (lambda (newlat L R)
                            (col (cons (car lat) newlat)
                                 L R)))))))

(define col1
  (lambda (lat L R) lat))
(define col2
  (lambda (lat L R) L))
(define col3
  (lambda (lat L R) R))

(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) col1)
(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) col2)
(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) col3)

;`evens-only` removes all the odd numbers

(define evens-only
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((even? (car l)) (cons (car l) (evens-only (cdr l))))
         (else
           (evens-only (cdr l)))))
      (else
        (cons (evens-only (car l)) (evens-only (cdr l)))))))

(evens-only '((9 1 2 8) 3 10 ((9 9) 7 6) 2)) 

; `evens-only&co` collects the list without odd numbers, product of even numbers and sum of odd numbers

(define evens-only&co
  (lambda (l col)
    (cond
      ((null? l)
       (col '() 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only&co (cdr l)
                          (lambda (newl p s)
                            (col (cons (car l) newl) (* (car l) p) s))))
         (else
           (evens-only&co (cdr l)
                           (lambda (newl p s)
                             (col newl p (+ (car l) s)))))))
      (else
        (evens-only&co (car l)
                        (lambda (al ap as)
                          (evens-only&co (cdr l)
                                          (lambda (dl dp ds)
                                            (col (cons al dl)
                                                 (* ap dp)
                                                 (+ as ds))))))))))

(define the-last-friend
  (lambda (e p s)
    (cons s (cons p e))))

(evens-only&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)