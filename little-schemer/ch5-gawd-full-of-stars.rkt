#lang racket


; atom? from book
(define atom? 
  (lambda (x) 
    (and (not (pair? x)) (not (null? x)))))
;; from ch4
(define myplus
  (lambda (n counter)
    (cond ((zero? counter) n)
          (else (myplus (add1 n) (sub1 counter))))))



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

; the book version
(define rember1*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
        (cond
            ((eq? (car l) a) (rember* a (cdr l)))
            (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))
    
     
(rember* 'a '((a b (f a) c) a (b a c) d))
(rember* 'cup '((coffee) cup ((tea) cup)))
(rember* ' cup '((coffee) cup ((tea) cup) (and (hick)) cup))
(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
(rember1* 'a '((a b c) c a ((c a) a b)))


(define how-much-wood 
   '((how much (wood))
     could
     ((a (wood) chuck))
     (((chuck)))
     (if (a) ((wood chuck)))
     could chuck wood) )


(define insertR*
    (lambda (new old l)
        (cond
            ((null? l) '())
            ((atom? (car l))
             (cond
                ((eq? (car l) old) 
                 (cons old (cons new (insertR* new old (cdr l)))))
                (else (cons (car l) (insertR* new old (cdr l))))))
            (else (cons
                    (insertR* new old (car l))
                    (insertR* new old (cdr l)))))))


;; trying this for fun, this adds numbers in 
;; an arbetrarly deeply nested list
(define add* 
    (lambda (tup)
        (cond 
            ((null? tup) 0)
            ((number? (car tup))
                (myplus (car tup) (add* (cdr tup)))) 
            (else (myplus
                    (add* (car tup))
                    (add* (cdr tup)))))))
(add* '(1 (2 (1 1) 3) 5 4)) ;=> 17
;;^^ you don't consider the case where (car tup) is a atom




