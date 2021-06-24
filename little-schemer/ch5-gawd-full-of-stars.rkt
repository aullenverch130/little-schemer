#lang racket

;(rember* a l)
;where a is cup and
;l is ((coffee) cup ((tea) cup)
; (and (hick)) cup)
;returns ((coffee) ((tea)) (and (hick)))
;Seems to remove a from every list in l
;this will require recursing through

(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      (else (cond
              ((atom? (car l)) 
                (cond 
                  ((eq? (car l) a) (rember* a (cdr l)))
                  (else (cons (car l) (rember* a (cdr l))))))
              (else (cons (rember* a (car l)) (rember* a (cdr l)))))))))


; (print '(rember* a (cdr l)))

;; the book version
(define rember1*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)))
      (cond
          ((eq? (car l) a) (rember* a (cdr l)))
          (else (cons (car l) (rember* a (cdr l)))))
        (else (cons (rember* a (car l)) (rember* a (cdr l)))))))




  
      
;; atom? from book
(define atom? 
  (lambda (x) 
    (and (not (pair? x)) (not (null? x)))))

     
(rember* 'a '((a b (f a) c) a (b a c) d))
(rember* 'cup '((coffee) cup ((tea) cup)))
(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))

(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))

