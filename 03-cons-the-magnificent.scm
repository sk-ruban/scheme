; `rember` removes the first instance of a member
; cons inserts the expression (car let) as the first item into (rember a (cdr lat))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
        (rember a (cdr lat)))))))

(rember 'bacon '(bacon lettuce tomato))
(rember 'and '(bacon lettuce and tomato))
(rember 'sauce '(soy sauce and tomato sauce))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The second commandment                                                     ;
;                                                                            ;
; Use /cons/ to build lists.                                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;`firsts` builds a list composed of only the first S-expression of each internal list

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l))
        (firsts (cdr l)))))))

(firsts '((a b) (c d) (e f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The third commandment                                                      ;
;                                                                            ;
; When building lists, describe the first typical element, and then /cons/   ;
; it onto the natural recursion.                                             ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; `insertR`  inserts a new atom to the right of the first occurrence of an old atom in a list 

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old (cons new (cdr lat))))
      (else
        (cons (car lat) (insertR new old (cdr lat)))))))

(insertR 'topping 'fudge '(ice cream fudge for dessert))

; `insertL`  inserts a new atom to the left of the first occurrence of an old atom in a list 

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new lat))
      (else
        (cons (car lat) (insertL new old (cdr lat)))))))

(insertL 'topping 'fudge '(ice cream fudge for dessert))

; `subst` replaces the first occurance of old with new in a list

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else 
        (cons (car lat) (subst new old (cdr lat)))))))

(subst 'topping 'fudge '(ice cream with fudge for dessert))

; `subst2` replaces the first occurance of o1 or o2 with new in a list

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1) (eq? (car lat) o2))
        (cons new (cdr lat)))
      (else 
        (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

  (subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))

; `multirember` removes all occurances of a member

  (define multirember
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((eq? (car lat) a) (multirember a (cdr lat)))
        (else (cons (car lat)
          (multirember a (cdr lat)))))))

(multirember 'cup '(coffee cup tea cup and hick cup))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else
        (cons (car lat) (multiinsertR new old (cdr lat)))))))

(multiinsertR 'topping 'fudge '(ice cream fudge for fudge))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else
        (cons (car lat) (multiinsertL new old (cdr lat)))))))

(multiinsertL 'fried 'fish '(chips and fish or fish and fried))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The fourth commandment (preliminary)                                       ;
;                                                                            ;
; Always change at least one argument while recurring. It must be changed to ;
; be closer to termination. The changing argument must be tested in the      ;
; termination condition: when using cdr, test the termination with null?.    ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else 
        (cons (car lat) (multisubst new old (cdr lat)))))))

(multisubst 'fried 'fish '(chips and fish or fish and fried))