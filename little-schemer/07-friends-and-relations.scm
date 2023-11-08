(define atom?
 (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else
        (cons (car lat) (multirember a (cdr lat)))))))

; no atom appears more than once in a set

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(set? '(apple peaches apple plum))
(set? '(apples peaches pears plums))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(makeset '(apple peach pear peach plum apple lemon peach))

(define makeset2
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat) (makeset2 (multirember (car lat) (cdr lat))))))))

(makeset2 '(apple peach pear peach plum apple lemon peach))
(makeset2 '(apple 3 pear 4 9 apple 3 4))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2) (subset? (cdr set1) set2))
      (else #f))))

(subset? '(5 chicken wings)
         '(5 hamburgers 2 pieces fried chicken and light duckling wings))
(subset? '(4 pounds of horseradish)
         '(four pounds of chicken and 5 ounces of horseradish))

; `subset2` is a shorter version of subset using `and`

(define subset2?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2)
                 (subset2? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set1 set2))))

(eqset? '(a b c) '(c b a)) 
(eqset? '(a b c) '(a b))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2) 
                (intersect? (cdr set1) set2))))))

(intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (cons (car set1) (intersect (cdr set1) set2)))
      (else
        (intersect (cdr set1) set2)))))

(intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(union '(stewed tomatoes and macaroni) '(macaroni and cheese))

; `xx` is the set difference function

(define xxx
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (xxx (cdr set1) set2))
      (else
        (cons (car set1) (xxx (cdr set1) set2))))))

; `intersectall` finds the intersect in a list of sets

(define intersectall
  (lambda (lat)
    (cond
      ((null? (cdr lat)) (car lat))
      (else (intersect (car lat) (intersectall (cdr lat)))))))

(intersectall '((a b c) (c a d e) (e f g h a b)))
(intersectall
  '((6 pears and)
    (3 peaches and 6 peppers)
    (8 pears and 6 plums)
    (and 6 prunes with some apples)))

; `pair` is a list of 2 S-expressions

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(a-pair? '(pear pear)) 

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

; relations are a set of pairs
; `fun` checks if the first of each pair is unique

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else
        (cons (car (car l)) (firsts (cdr l)))))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(fun? '((4 3) (4 2) (7 6) (6 2) (3 4)))     ; #f
(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))     ; #t

; `revrel` reverses the order within pairs

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (build (second (car rel)) (first (car rel)))
            (revrel (cdr rel)))))))

(revrel '((8 a) (pumpkin pie) (got sick)))

; `revrel` can be simplified further using `revpair`

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel)) (revrel (cdr rel)))))))

; `fullfun` checks if the second of each pair is unique

(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else
        (cons (second (car l)) (seconds (cdr l)))))))

(define fullfun?
  (lambda (rel)
    (set? (seconds rel))))

(fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
(fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))
(fullfun? '((grape raisin)
            (plum prune)
            (stewed prune)))

; `fullfun` can be simplified with `revrel`

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

(one-to-one? '((8 3) (4 2) (7 6) (6 2) (3 4)))
(one-to-one? '((8 3) (4 8) (7 6) (6 2) (3 4)))
(one-to-one? '((grape raisin)
               (plum prune)
               (stewed prune)))