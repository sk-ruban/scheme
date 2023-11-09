(+ 137 349)
(* 5 99)
(+ 2.7 10)
; prefix notation allows for an arbitary number of arguments
(+ 21 35 12 7) 
(+ (* 3 5) (- 10 6))

(define size 2) ; name things with define (abstraction)
size
(* 5 size)
; definitions are stored in the global environment

(define (square x) (* x x)) ; compound procedure
(square 21)
(square (+ 2 5))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(sum-of-squares 3 4) 
; scheme uses applicative-order-evaluation 
; - inner most functions are evaluated first

(define (abs x)
  (cond ((> x 0) x) ; (cond (<predicate> <consequent expression>))
        ((= x 0) 0)
        ((< x 0) (- x))))
(define (abs2 x)
  (cond ((< x 0) (-x))
        (else x)))
; (if <predicate> <consequent> <alternative>)
(define (abs3 x)
  (if (< x 0) (-x)
      x))

(define (>= x y)
  (not (< x y)))
(define (>= x y)
  (or (= x y) (> x y)))

; exercise 1.2
(/ (+ 5 (+ 4 (- 2 (- 3 (+ 6 0.8))))) (* 3 (- 2 7) (- 6 2)))

; exercise 1.3
(define (sum-of-larger-squares x y z)
  (cond ((and (< z y) (< z x)) (sum-of-squares x y))
        ((and (< x y) (< x z)) (sum-of-squares z y))
        ((sum-of-squares z x))))
(sum-of-larger-squares 3 6 4)

; exercise 1.4
; if b>0, return a+b else a-b

; exercise 1.5
; normal-order eval: 0
; applicative-order eval: infinite recursion due to (p)

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(sqrt 9)
(sqrt (+ 100 37))

; exercise 1.6
; scheme uses applicative order evaluation that means that both the then-clause and else-clause would be evaluated before conditional is called in the new-if procedure

; exercise 1.8
(define (cube-root x)
  (cube-iter 1.0 x))
(define (cube-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-iter (improve guess x)
                 x)))
(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))
(define (cube x)
  (* x x x))
(define (improve guess x)
  (/ (+ (/ x (square guess)) 
        (* 2 guess)) 
     3))
(cube 3)

; bound variables can only be referenced within the scope

; lexical scoping shortens the function as x only needs to be defined at the top
; block structure packages all smaller functions in the body of the main function
(define (sqrt x)
(define (good-enough? guess)
  (< (abs (- (square guess) x)) 0.001))
(define (improve guess)
  (average guess (/ x guess)))
(define (sqrt-iter guess)
  (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
(sqrt-iter 1.0))
(sqrt 9)