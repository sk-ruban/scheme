; supplement for understanding the Y-combinator: https://mvanier.livejournal.com/2897.html

; a bound variable is simply a variable which is contained inside the 
; body of a lambda expression that has that variable name as one of its arguments
; a lambda expression is a combinator if and only if it contains no free variables

; the function below is NOT a combinator as `factorial` is a free variable
; in fact, the names =, *, and - are also free variables

(lambda (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

; original function

(define factorial
  (lambda (n)
    (if (= n 0)
        1
        (* n (factorial (- n 1))))))

; we want to remove the recursive call to factorial

(define sort-of-factorial
  (lambda (n)
    (if (= n 0)
        1
        (* n (<???> (- n 1))))))

; TRICK - we can abstract it out and make it a parameter of the function
; `almost-factorial` is now a higher order function which takes a single argument f
; which is a function and returns another function

(define almost-factorial
  (lambda (f)
    (lambda (n)
      (if (= n 0)
          1
          (* n (f (- n 1)))))))

; converting a fibonacci function using the method above

(define fibonacci
  (lambda (n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))))

(define almost-fibonacci
  (lambda (f)
    (lambda (n)
      (cond ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (f (- n 1)) (f (- n 2))))))))

; let's consider the following:

(define factorialB (almost-factorial factorialA))

(define factorialB
  ((lambda (f)
     (lambda (n)
       (if (= n 0)
           1
           (* n (f (- n 1))))))
   factorialA))

; substituing factorialA for f:
; this is now a non-recursive function but the problem is we now need to define factorialA

(define factorialB
  (lambda (n)
    (if (= n 0)
        1
        (* n (factorialA (- n 1))))))

(define identity (lambda (x) x)) ; combinator which returns its argument
(define factorial0 (almost-factorial identity)) ; can only compute the factorial for 0

(factorial0 0)

((almost-factorial identity) 0)

(((lambda (f)
    (lambda (n)
       (if (= n 0) 1
           (* n (f (- n 1))))))
  identity)
  0)

((lambda (n)
   (if (= n 0) 1
       (* n (identity (- n 1)))))
  0)

(if (= 0 0) 1
    (* 0 (identity (- 0 1))))

; unfortunately, it won't work for n > 0
; unless...

(define factorial1 (almost-factorial factorial0))

; unfortunately, it won't work for n > 1
; unless...

(define factorial2 (almost-factorial factorial1))
(define factorial3 (almost-factorial factorial2))
(define factorial4 (almost-factorial factorial3))
(define factorial5 (almost-factorial factorial4))

; a fixpoint is a value where the input = output

fixpoint-function = (almost-factorial fixpoint-function)

; Y is known as the fixpoint combinator. It takes in a function and returns its fixpoint

(Y f) = fixpoint-of-f
(f fixpoint-of-f) = fixpoint-of-f
(Y f) = fixpoint-of-f = (f fixpoint-of-f)
(Y f) = (f (Y f))

; this trick works for lazy languages
; lazy evaluation means that in order to evaluate an expression in the language, 
; you only evaluate as much of the expression as is needed to get the final result

(define Y
  (lambda (f)
    (f (Y f))))

; for strict languages the following equality will hold

(Y f) = (lambda (x) ((Y f) x))

(define Y
  (lambda (f)
    (f (lambda (x) ((Y f) x)))))

(define almost-factorial
  (lambda (f)
    (lambda (n)
      (if (= n 0)
          1
          (* n (f (- n 1)))))))

(define factorial (Y almost-factorial))

; expanding:

(define factorial
  ((lambda (f) (f (lambda (x) ((Y f) x))))
   almost-factorial))

(define factorial
    (almost-factorial (lambda (x) ((Y almost-factorial) x))))