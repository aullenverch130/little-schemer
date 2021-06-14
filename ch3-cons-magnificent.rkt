#lang racket

;; ex: from book that removes 1st occurance of a in lat
;; but it doesn't work for the cases in which a is not
;; equal to first atom of lat
(define rember1
  (lambda (a lat)
    (cond 
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (rember1 a (cdr lat))))))))


;; 2nd ex: of func that returns 
;; 1st occurance of a in lat /w cons
(define rember2
  (lambda (a lat)
    (cond 
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (cons (car lat)
                      (rember2 a (cdr lat)))))))))


;; simplification of 2nd ex:
(define rember-simp
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
              (rember-simp a (cdr lat)))))))
;; ^^ func structure doesn't match arg structure??

;; define #rember-simp as function of a & lat:
;;
;; #rember-simp is func of a & lat: <- could imply "&" w/ ","
;;      if lat is empty, return empty list (???)
;;      if 1st of lat & a equals true, 
;;              return #rember-simp of (rest of lat) & a
;;      else return 1st of lat with(cons???), <- imply "then"
;;         imply return? -> #rember-simp of (rest of lat) & a
;; ^^ lambda eng ex:
;; ^^ maybe instead of return, evaluate
;; ^^ perhaps instead of doing "if, then" for cases could do 
;; if (______, _______), or (______, ______), (______, ______), 
;;    (______, then ______),  else (______, _______)
;; ^^ commas, and comma types could all be implied as default.. 
;; read : as "is" or "as" associate.. 

;; #rember-simp is func of a & lat: 
;;      if lat is empty, empty
;;      or if 1st of lat equals a, 
;;              #rember-simp of (rest of lat) & a
;;      else 1st of lat,
;;            with #rember-simp of (rest of lat) & a

;; #rember-simp is func of a & lat: 
;;      if lat is empty, is empty
;;      or if 1st of lat equals a, 
;;              is #rember-simp of (rest of lat) & a
;;      or else is 1st of lat,
;;            with #rember-simp of (rest of lat) & a

;; this is to test for writing "firsts" func
;; it converts '(a b) to (cons a (cons b '())) then evals
(define deconstruct (lambda (x)
    (cond
      ((null? x) '())
      (else (cons (car x) (deconstruct (cdr x)))))))

;; my firsts func
(define firsts (lambda (x)
    (cond
      ((null? x) '())
      (else (cons (car (car x)) (firsts (cdr x)))))))

;; testing firsts
(firsts '((a b) (b c) (c d))) ;=> (a b c)   
(firsts '((apple peach pumpkin)
          (plum pear cherry)
          (grape raisin pea)
          (bean carrot eggplant))) ;=> (apple plum grape bean)

;; test for seconds func
(car (cdr (car '((a b c) b c)))) ;=> b
;; seconds func
(define seconds (lambda (x)
    (cond
      ((null? x) '())
      (else (cons (car (cdr (car x))) (seconds (cdr x)))))))
(seconds '((a b)(c d)(e f))) ;=> (b d f)


;; category theory idea
;;
;; empty list = initial obj
;; cons = coproduct
;;
;; ??? = terminal obj
;; list = product ??


;; my try @ insertR
(define insertR (lambda (new old lat)
  (cond 
    ((null? lat) '())
    ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
    (else (cons (car lat) (insertR new old (cdr lat)))))))
(insertR 'e 'd '(a b c d f g d h)) ;=> (a b c d e f g h)

;; the work up to insertR from book that I'm calling replace
(define replace (lambda (new old lat)
  (cond 
    ((null? lat) '())
    ((eq? (car lat) old) (cons new (cdr lat)))
    (else (cons (car lat) (replace new old (cdr lat)))))))
(replace 'topping 'fudge '(ice cream with fudge for dessert))
;=> (ice cream with topping for dessert)

;; attempt @ insertL
(define insertL 
  (lambda (new old lat) 
    (cond 
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cons old (cdr lat))))
      (else (cons (car lat) (insertL new old (cdr lat)))))))
(insertL 'topping 'fudge '(ice cream with fudge for dessert))
;=> (ice cream with topping fudge for dessert)
;; ^^ correct!! or could of done (cons new lat) !!!

;; subst is what I called replace....


;; trying subst2, replaces first occurance of o1 or o2 with new 
(define subst2 
  (lambda (new o1 o2 lat)
    (cond 
      ((null? lat) '())
      ((eq? (car lat) o1) (cons new (cdr lat)))
      ((eq? (car lat) o2) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))
(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
;=> (vanilla ice cream with chocolate topping)

;; alternative way to do it.. 
; (define subst2alt
;   (lambda (new o1 o2 lat)
;     (cond 
;       ((null? lat) '())
;       (or (eq? (car lat) o1) (eq? (car lat) o2) (cons new (cdr lat)))
;       (else (cons (car lat) (subst2alt new o1 o2 (cdr lat)))))))
; (subst2alt 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
;; ^^ or doesn't work in racket here??

;; the book told me to "Go cons a cake onto my mouth"... lol
(cons 'cake '(mouth))

;; tying multiremeber.. it removes all a in lat
(define multirember 
  (lambda (a lat)
    (cond 
      ((null? lat) '())
      (else
        (cond 
          ((eq? (car lat) a) (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))))
(multirember 'cup '(coffee cup tea cup and hick cup))

;; first attempt @ multiinsertR, 
;; prob could have done old instead of (car lat)... && in same place as new
(define multiinsetR
  (lambda (new old lat)
    (cond 
      ((null? lat) '())
      (else
        (cond
          ((eq? (car lat) old) (cons (car lat) 
                              (multiinsetR new old (cons new (cdr lat)))))
          (else (cons (car lat) (multiinsetR new old (cdr lat)))))))))
(multiinsetR 'B 'b '(a b b a)) ;=> (a b B b B a)
(multiinsetR 'fried 'fish '(chips and fish or fish and fried))
;=> (chips and fish fried or fish fried and fried)
;; ^^ so I can get away with consing new in the func call here 
;; because it will get consed on as the car of the next func call
;; becasue new will cause an else for the next funct call

;; my try @ multiinsetL
(define multiinsetL
  (lambda (new old lat)
    (cond 
      ((null? lat) '())
      (else
        (cond
          ((eq? (car lat) old) (cons new (cons old 
                                    (multiinsetL new old (cdr lat)))))
          (else (cons (car lat) (multiinsetL new old (cdr lat)))))))))
(multiinsetL 'fried 'fish '(chips and fish or fish and fried))
;=> (chips and fried fish or fried fish and fried)

;; multisubst just conses new instead of (car lat) or old..
(define multisubst
  (lambda (new old lat)
    (cond 
      ((null? lat) '())
      ((eq? (car lat) old)
                (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))
(multisubst 'fried 'fish '(chips and fish or fish and fried))
;=> (chips and fried or fried and fried)

