(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define atom?
 (lambda (x)
    (and (not (pair? x)) (not (null? x))))) 

; `pick` returns the n-th element in a lat

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else
        (pick (sub1 n) (cdr lat))))))

; 'looking` keeps moving to the index of the number picked
; it is a partial function which may recur indefinitly

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

; `sorn` stands for a symbol or number

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      (else (eq? sorn a )))))

(looking 'caviar '(6 2 4 caviar 5 7 3))      ; #t
(looking 'caviar '(6 2 grits caviar 5 7 3))  ; #f because of grits

; `eternity` - another partial function which never reaches its goal

(define eternity
  (lambda (x)
    (eternity x)))

; `shift` shifts the second part of the first component into the second component

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define shift
  (lambda (pair)
    (build (first (first pair))
      (build (second (first pair))
        (second pair)))))

(shift '((a b) c)) 
(shift '((a b) (c d))) 

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

; `align` is not a partial function as it yields a value for every argument

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (align (shift pora)))
      (else (build (first pora) (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
        (+ (length* (first pora))
           (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
        (+ (* (weight* (first pora)) 2)
           (weight* (second pora)))))))

(weight* '((a b) c)) ; (2+1)*2 + 1 = 7
(weight* '(a (b c))) ; (1*2) + (2+1) = 5

; `shuffle` swaps the components of the pairs if the first component is a pair
; it is not a total function as the swap indefinitely in some cases

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (shuffle (revpair pora)))
      (else
        (build (first pora) (shuffle (second pora)))))))

(shuffle '(a (b c)))
(shuffle '(a b))
(shuffle '((a b) (c d))) ; infinite swap

(define one?
  (lambda (n) (= n 1)))

; Not sure if `C` is a total function (Collatz Conjecture)

(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      (else
        (cond
          ((even? n) (C (/ n 2)))
          (else
            (C (add1 (* 3 n)))))))))

; Ackermann Function

(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else
        (A (sub1 n) (A n (sub1 m)))))))

(A 1 0) ; 2
(A 1 1) ; = (A 0 (A 1 0)) = (A 0 2) = 3

; length0 - the function below can only determine the length of an empty list
; a function without a name (define ...) is called a lambda expression
; scheme allows the use of "first class functions" - passing a function 
; into another function as an argument

(lambda (l)
  (cond
    ((null? l) 0)
    (else
      (add1 (eternity (cdr l))))))

; the function below can only determine the length of a list <=1
; we simply replaced `eternity` with the definition above

(lambda (l)
  (cond
    ((null? l) 0)
    (else
      (add1
        ((lambda(l)
           (cond
             ((null? l) 0)
             (else
               (add1 (eternity (cdr l))))))
         (cdr l))))))

; rewriting length0

((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 eternity)

; rewriting length<=1

((lambda (f)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (f (cdr l)))))))
 ((lambda (g)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (g (cdr l)))))))
  eternity))

; compacting the above using mk-length

(lambda (mk-length)
  (mk-length eternity))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else
         (add1
           ((mk-length eternity) (cdr l))))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else
            (add1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))

; applicative-order Y combinator
; Y combinator is a higher order function which takes a function that isn't recursive
; and returns a version of the function that is recursive

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))