; prerequisite functions

(define atom?
 (lambda (x)
    (and (not (pair? x)) (not (null? x)))))  

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

; an entry is a pair of lists where the first list is a set - like a dictionary

'((appetizer entree bevarage)
  (beer beer beer))

; looks up an entry to find the value by name
; entry-f is the response when key is missing

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help
      name
      (first entry)
      (second entry)
      entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else
        (lookup-in-entry-help
          name
          (cdr names)
          (cdr values)
          entry-f)))))

(lookup-in-entry
  'entree
  '((appetizer entree bevarage) (pate boeuf vin))
  (lambda (n) '()))

(lookup-in-entry
  'no-such-item
  '((appetizer entree bevarage) (pate boeuf vin))
  (lambda (n) '()))

; a table is a list of entries

'(((appetizer entree beverage) (pate boeuf vin))
  ((beverage dessert) ((food is) (number one with us))))

; `extend-table` adds an entry to a table

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else
        (lookup-in-entry
          name
          (car table)
          (lambda (name)
            (lookup-in-table
              name
              (cdr table)
              table-f)))))))

(lookup-in-table
  'beverage
  '(((entree dessert) (spaghetti spumoni))
    ((appetizer entree beverage) (food tastes good)))
  (lambda (n) '()))

; there are 6 types - *const, *quote, *identifier, *lambda, *cond, *application

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else
        (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number?) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) 'quote) *quote)
         ((eq? (car e) 'lambda) *lambda)
         ((eq? (car e) 'cond) *cond)
         (else *application)))
      (else *application))))

; value function takes an expression and evaulates it

(define value
  (lambda (e)
    (meaning e '())))

; meaning function translates an expression to its meaning

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

; for numbers and #t or #f it just returns the expression
; all other atoms are primitives

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else
        (build 'primitive e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car '()))) 

(define *lambda
  (lambda (e table)
    (build 'non-primitive
           (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else
        (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x 'else))
      (else #f))))

(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlis
  (lambda (args table)
    (cond
      ((null? args) '())
      (else
        (cons (meaning (car args) table)
              (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (applyz
      (meaning (function-of e) table)
      (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

(define apply*
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive (second fun) vals))
      ((non-primitive? fun)
       (apply-closure (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name 'cons)
       (cons (first vals) (second vals)))
      ((eq? name 'car)
       (car (first vals)))
      ((eq? name 'cdr)
       (cdr (first vals)))
      ((eq? name 'null?)
       (null? (first vals)))
      ((eq? name 'eq?)
       (eq? (first vals) (second vals)))
      ((eq? name 'atom?)
       (:atom? (first vals)))
      ((eq? name 'zero?)
       (zero? (first vals)))
      ((eq? name 'add1)
       (+ 1 (first vals)))
      ((eq? name 'sub1)
       (- 1 (first vals)))
      ((eq? name 'number?)
       (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) 'primitive) #t)
      ((eq? (car x) 'non-primitive) #t)
      (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning
      (body-of closure)
      (extend-table (new-entry
                      (formals-of closure)
                      vals)
                    (table-of closure)))))