; all whole positive numbers are atoms 

(define add1
  (lambda (n)
    (+ n 1)))

(add1 67)

(define sub1
  (lambda (n)
    (- n 1)))

(sub1 5)
(sub1 0)

; o+ adds two numbers

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(o+ 46 12)

; o- subtracts one number from another

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

(o- 14 3)

; tuples are lists of ONLY numbers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The first commandment (first revision)                                     ;
;                                                                            ;
; When recurring on a list of atoms, lat, ask two questions about it:        ;
; (null? lat) and else.                                                      ;
; When recurring on a number, n, ask two questions about it: (zero? n) and   ;
; else.                                                                      ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; addtup adds all the numbers in a tuple

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtup (cdr tup)))))))

(addtup '(15 6 7 12 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The fourth commandment (first revision)                                    ;
;                                                                            ;
; Always change at least one argument while recurring. It must be changed to ;
; be closer to termination. The changing argument must be tested in the      ;
; termination condition:                                                     ;
; when using cdr, test the termination with null? and                        ;
; when using sub1, test termination with zero?.                              ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; o* multiplies two numbers

(define o*
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+  n (o* n (sub1 m)))))))

(o* 3 5)
(o* 12 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The fifth commandment                                                      ;
;                                                                            ;
; When building a value with o+, always use 0 for the value of the           ;
; terminating line, for adding 0 does not change the value of an addition.   ;
;                                                                            ;
; When building a value with o*, always use 1 for the value of the           ;
; terminating line, for multiplying by 1 does not change the value of a      ;
; multiplication.                                                            ;
;                                                                            ;
; When building a value with cons, always consider () for the value of the   ;
; terminating line.                                                          ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; tup+ adds two tuples index-wise

(define tup+
  (lambda (tup1 tup2)
  (cond
    ((null? tup1) tup2)
    ((null? tup2) tup1)
    (else (cons (o+ (car tup1) (car tup2))
          (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(3 6 9 11 4) '(8 5 2 0 7))
(tup+ '(3 7 8 1) '(4 6)) 

; (> n m) checks if n is greater than m

(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(o> 12 133)
(o> 3 3)

(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

(o< 12 133)
(o< 3 3)

(define o=
  (lambda (n m)
    (cond
      ((o< m n) #f)
      ((o> m n) #f)
      (else #t))))

(o= 3 3)
(o= 12 133)

; o^ computes n^m

(define o^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (o* n (o^ n (sub1 m)))))))

(o^ 2 5)

(define o/
  (lambda (n m)
    (cond
      ((o< n m) 0)
      (else (add1 (o/ (o- n m) m))))))

(o/ 15 4) 

; length counts the number of atoms in a list

(define olength
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (olength (cdr lat)))))))

(olength '(ham and cheese on rye))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(pick 4 '(lasagna spaghetti ravioli macaroni meatball))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 3 '(hotdogs with hot mustard))

; no-nums returns a list with all numbers removed

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(no-nums '(5 pears 6 prunes 9 dates))

; all-nums returns a list with only numbers contained

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(all-nums '(5 pears 6 prunes 9 dates))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (o= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(eqan? 3 3)     ; #t
(eqan? 3 4)     ; #f
(eqan? 'a 'a)   ; #t
(eqan? 'a 'b)   ; #f

; occur counts the number of times an atom a appears in a lat

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? (car lat) a) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(occur 'x '(a b x x c d x)) 
(occur 'x '()) 

(define one?
  (lambda (n) (= n 1)))

(one? 5)  

(define rempick-one
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick-one (sub1 n) (cdr lat)))))))

(rempick-one 3 '(lemon meringue salty pie))