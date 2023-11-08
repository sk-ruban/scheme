(define atom?                                                                
 (lambda (x)                                                                 
    (and (not (pair? x)) (not (null? x))))) 

(define add1
  (lambda (n)
    (+ n 1)))

; `rember*` removes all instances of a member
; `(rember* a (car l)` helps to recur inside a list

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? (car l) a)
          (rember* a (cdr l)))
        (else
          (cons (car l) (rember* a (cdr l))))))
      (else
        (cons (rember* a (car l)) (rember* a (cdr l)))))))

(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))

; 'insertR*' inserts the atom new to the right of the atom old regardless of where old occurs

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? (car l) old)
           (cons old (cons new (insertR* new old (cdr l)))))
        (else
          (cons (car l) (insertR* new old (cdr l))))))
      (else
        (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck)))
    (if (a) ((wood chuck))) could chuck wood))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The first commandment (final version)                                      ;
;                                                                            ;
; When recurring on a list of atoms, lat, ask two questions about it:        ;
; (null? lat) and else.                                                      ;
; When recurring on a number, n, ask two questions about it: (zero? n) and   ;
; else.                                                                      ;
; When recurring on a list of S-expressions, l, ask three questions about    ;
; it: (null? l), (atom? (car l)), and else.                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The fourth commandment (final version)                                     ;
;                                                                            ;
; Always change at least one argument while recurring. When recurring on a   ;
; list of atoms, lat, use (cdr l). When recurring on a number, n, use        ;
; (sub1 n). And when recurring on a list of S-expressions, l, use (car l)    ;
; and (cdr l) if neither (null? l) nor (atom? (car l)) are true.             ;
;                                                                            ;
; It must be changed to be closer to termination. The changing argument must ;
; be tested in the termination condition:                                    ;
; * when using cdr, test the termination with null? and                      ;
; * when using sub1, test termination with zero?.                            ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
        (cond
          ((eq? (car l) a) 
           (add1 (occur* a (cdr l))))
        (else 
          (occur* a (cdr l)))))
      (else
        (+ (occur* a (car l))
           (occur* a (cdr l)))))))

(occur*
  'banana
  '((banana)
    (split ((((banana ice)))
            (cream (banana))
            sherbet))
    (banana)
    (bread)
    (banana brandy)))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (subst* new old (cdr l))))
         (else
           (cons (car l) (subst* new old (cdr l))))))
      (else
        (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

  (subst*
  'orange
  'banana
  '((banana)
    (split ((((banana ice)))
            (cream (banana))
            sherbet))
    (banana)
    (bread)
    (banana brandy)))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (cons old (insertL* new old (cdr l)))))
         (else
           (cons (car l) (insertL* new old (cdr l))))))
      (else
        (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(insertL*
  'pecker
  'chuck
  '((how much (wood)) could ((a (wood) chuck)) (((chuck)))
    (if (a) ((wood chuck))) could chuck wood))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? (car l) a)
           (member* a (cdr l))))
      (else
        (or (member* a (car l))
            (member* a (cdr l)))))))

(member*
  'chips
  '((potato) (chips ((with) fish) (chips))))

; `leftmost` finds the leftmost atom in a non-empty list

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(leftmost '((potato) (chips ((with) fish) (chips))))

; `eqlist` checks if two lists are the same

(define eqlist?
  (lambda (l1 l2)
    (cond
      ; case 1: l1 is empty, l2 is empty, atom, list
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ; case 2: l1 is atom, l2 is empty, atom, list
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eq? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      ; case 3: l1 is a list, l2 is empty, atom, list
      (else
        (and (eqlist? (car l1) (car l2))
             (eqlist? (cdr l1) (cdr l2)))))))

(eqlist? '(strawberry ice cream) '(strawberry ice cream))    
(eqlist? '(strawberry ice cream) '(strawberry cream ice))
(eqlist? '(banan ((split))) '((banana) split))

(define *equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eq? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

(*equal? 'a 'a)   
(*equal? '(a b c) '(a b c))
(*equal? '(a (b c)) '(a (b c)))

(define eqlist2?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
        (and (*equal? (car l1) (car l2))
             (*equal? (cdr l1) (cdr l2)))))))

(eqlist2? '(strawberry ice cream) '(strawberry ice cream))
(eqlist2? '(strawberry ice cream) '(strawberry cream ice))
(eqlist2? '(banan ((split))) '((banana) split))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The sixth commandment                                                      ;
;                                                                            ;
; Simplify only after the function is correct.                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rember2
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (cdr l))
      (else (cons (car l) (rember2 s (cdr l)))))))

(rember2 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))