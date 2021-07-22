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


(define makeset1
    (lambda (lat)
        (cond 
            ((null? lat) '())
            (else (cons (car lat) 
                         (makeset1 (multirember (car lat) (cdr lat))))))))
(makeset1 '(apple peach pear peach plum apple lemon peach))




