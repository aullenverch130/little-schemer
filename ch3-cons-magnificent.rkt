#lang racket

;; ex: from book that removes 1st occurance of a in lat
;; but it doesn't work for the cases in which a is not
;; equal to first atom of lat
(define rember1
  (lambda (a lat)
    (cond 
      ((null? lat) (quote ())
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (rember1 a (cdr lat)))))))))


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
;; TODO: if 1st of lat & a equal true, 
;;              call #rember-simp with rest of lat & a
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

;; TODO: write 2nds func!


;; category theory idea
;;
;; empty list = initial obj
;; cons = coproduct
;;
;; ??? = terminal obj
;; list = product ??
