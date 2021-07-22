#lang racket

; atom? from book
(define atom? 
  (lambda (x) 
    (and (not (pair? x)) (not (null? x)))))
;; from ch. 2
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))
;; it removes all a in lat, from ch. 3
(define multirember 
  (lambda (a lat)
    (cond 
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))
(multirember 'cup '(coffee cup tea cup and hick cup))













;; my try at set? 
(define set?
    (lambda (lat)
        (cond
            ((null? lat) #t)
            ((member? (car lat) (cdr lat)) #f)
            (else (set? (cdr lat))))))

;; my try at makeset
(define makeset
    (lambda (lat) 
        (cond 
            ((null? lat) '())
            ((member? (car lat) (cdr lat)) 
                (makeset (cdr lat)))
            (else (cons (car lat) (makeset (cdr lat)))))))
(makeset '(apple peach pear peach plum apple lemon peach))
; => '(pear plum apple lemon peach)
;;^^ my version returns opposite order but still works..
;; ^^ the same order found in book.

;; makeset w/ multirember
(define makeset1
    (lambda (lat)
        (cond 
            ((null? lat) '())
            (else (cons (car lat) 
                         (makeset1 (multirember (car lat) (cdr lat))))))))
(makeset1 '(apple peach pear peach plum apple lemon peach))


;; try subset?
(define subset?
    (lambda (set1 set2)
        (cond
            ((null? set1) #t)
            ((member? (car set1) set2) 
                (subset? (cdr set1) set2))
            (else #f))))
(subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings))

;; ^^ I already wrote the "shorter" version

;; trying subset? w/ and
(define subset2?
    (lambda (set1 set2)
        (cond 
            ((null? set1) #t)
            (else 
              (and (member? (car set1) set2)
                   (subset2? (cdr set1) set2))))))
(subset2? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings))


;; trying eqset?
(define eqset?
    (lambda (set1 set2)
        (cond
            ((and (subset? set1 set2)
                  (subset? set2 set1)) #t)
            (else #f))))
(eqset? '(6 large chickens with wings) '(6 chickens with large wings))

;; trying 1 cond line eqset
(define eqset?1ln
    (lambda (set1 set2)
        (and (subset? set1 set2)
             (subset? set2 set1))))
(eqset?1ln '(6 large chickens with wings) '(6 chickens with large wings))
;; ^^ ended did the "one liner"

;; trying intersect? ,  wrote the shorter version
(define intersect? 
    (lambda (set1 set2)
        (cond
            ((null? set1) #f)
            ((member? (car set1) set2) #t)
            (else (intersect? (cdr set1) set2)))))
(intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese))

;; trying intersect? w/ or
(define intersect?or
    (lambda (set1 set2)
        (cond
            ((null? set1) #f)
            (else 
              (or (member? (car set1) set2)
                  (intersect?or (cdr set1) set2))))))
(intersect?or '(stewed tomatoes and macaroni) '(macaroni and cheese))

;; interesting, intersect? & subset? are the same except for using or vs and!!

;; trying intersect
(define intersect
    (lambda (set1 set2)
        (cond 
            ((null? set1) '())
            ((member? (car set1) set2) 
                (cons (car set1) (intersect (cdr set1) set2)))
            (else 
                (intersect (cdr set1) set2)))))
(intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))

;; trying union
(define union
    (lambda (set1 set2)
        (cond
            ((null? set1) set2)
            ((member? (car set1) set2)
                (union (cdr set1) set2))
            (else 
                (cons (car set1) (union (cdr set1) set2))))))
(union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))


;; the func showed is the "not subset" func
(define notsubset
    (lambda (set1 set2)
        (cond
            ((null? set1) '())
            ((member? (car set1) set2) 
                (notsubset (cdr set1) set2))
            (else (cons (car set1) 
                    (notsubset (cdr set1) set2))))))
;; aka union without adding set2 at the end..
(notsubset '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))

;; writing intersect-all
(define intersect-all
    (lambda (l-set)
        (cond
            ((null? (cdr l-set)) (car l-set))
            (else (intersect (car l-set) (intersect-all (cdr l-set)))))))
(intersect-all '((a b c) (c a d e) (e f g h a b)))
;; using intersect like a cons..

;; a-pair
(define a-pair
    (lambda (l)
        (cond
            ((or (null? l) (atom? l)) #f)
            ((null? (cdr l)) #f)
            ((null? (cdr (cdr l))) #t)
            (else #f))))
(print "a-pair test")
(a-pair '(full (house)))

;; one liner first, second, & build
(define first
    (lambda (p)
        (car p)))
(define second
    (lambda (p)
        (car (cdr p))))
(define build
    (lambda (s1 s2)
        (cons s1 (cons s2 '()))))
;; my one liner third func
(define third
    (lambda (p)
        (car (cdr (cdr p)))))

;; firsts from ch. 3
(define firsts (lambda (x)
    (cond
      ((null? x) '())
      (else (cons (car (car x)) (firsts (cdr x)))))))
(firsts '((a b) (b c) (c d))) ;=> (a b c)   

;; seconds helper func
(define seconds (lambda (x)
    (cond
      ((null? x) '())
      (else (cons (car (cdr (car x))) (seconds (cdr x)))))))
(seconds '((a b) (b c) (c d))) ;=> (b c d)   

;; writing "fun?", funcs can't have one-to-many rel
(define fun? 
    (lambda (x)
        (set? (firsts x))))
(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
(fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))

;; writing revrel!
(define revrel
    (lambda (rel)
        (cond
            ((null? rel) '())
            (else (cons (build (second (car rel)) (first (car rel)))
                        (revrel (cdr rel)))))))
(revrel '((8 a) (pumpkin pie) (got sick)))

;; the helper revpair func from book
(define revpair
    (lambda (pair)
        (build (second pair) (first pair))))

;; revrel w/ revpair
(define revrel1
    (lambda (rel)
        (cond
            ((null? rel) '())
            (else (cons (revpair (car rel))
                        (revrel (cdr rel)))))))
(revrel1 '((8 a) (pumpkin pie) (got sick)))

;; shouldn't fullfun? be called faithfulfun?
;; it's looking for injective func, not surjective func..
;; ^^ correct, another name is one-to-one

;; define fullfun? injectfun / faithfulfun
(define faithfulfun?
    (lambda (rel)
        (and (fun? rel)
             (set? (seconds rel)))))
(print "faithful fun tests") (newline)
(faithfulfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
(faithfulfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))


;; write one-to-one, I'll call it injectfun
(define injectfun?
    (lambda (fun)
        (fun? (revrel fun))))
(injectfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
(injectfun? '((chocolate chip) (doughy cookie)))

;; how would you do surjective!? maybe use subset? func... ?

;; when this ch. references representables are they representable functors??



