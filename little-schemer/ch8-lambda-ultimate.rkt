#lang racket

; atom? from book
(define atom? 
  (lambda (x) 
    (and (not (pair? x)) (not (null? x)))))

;; wrote before looking @ books ex:
(define my= 
    (lambda (m n)
        (cond
            ((and (zero? m) (zero? n)) #t)
            ((or (zero? m) (zero? n)) #f)
            (else (my= (sub1 m) (sub1 n))))))


(define equal? 
  (lambda (s1 s2)
    (cond 
      ((and (atom? s1) (atom? s2))
        (eq? s1 s2))
      ((atom? s1) #f)
      ((atom? s2) #f)
      (else 
        (eqlist? s1 s2)))))


(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t) ;<-- it got to the end
      ((or (null? l1) (null? l2)) #f)

      (else 
        (and (equal? (car l1) (car l2)) 
             (equal? (cdr l1) (cdr l2)))))))
;; from ch. 5
(define rember
  (lambda (s l)
    (cond 
      ((null? l) '())
      ((equal? (car l) s) (cdr l))
      (else (cons (car l) (rember s (cdr l)))))))
(rember '(a) '((b) (a) c)) ;=> '((b) c)



;; trying rember-f
; (define rember-f
;     (lambda (test? a l)
;         (test? a l)))

;; trying rember-f
(define rember-f
  (lambda (test? a l)
    (cond 
      ((null? l) '())
      ((test? (car l) a) (cdr l))
      (else (cons (car l) (rember-f test? a (cdr l)))))))
(rember-f eq? 'a '((b) a c)) ;=> '((b) c)
(rember-f my= 5 '(6 2 5 3))

;; curry
(define eq?-c 
    (lambda (x)
        (lambda (y)
            (eq? x y))))

(define eq?-salad (eq?-c 'salad))
eq?-salad
(eq?-salad 'salad)
(eq?-salad 'tomato)

(print "test w/o defining func") (newline)
((eq?-c 'a) 'a)
((eq?-c 'a) 'b)


(define rember-f1 
    (lambda (test?)
     (lambda (s l)
        (cond 
            ((null? l) '())
            ((test? (car l) s) (cdr l))
            (else (cons (car l) ((rember-f1 test?) s (cdr l))))))))

(define rember-f2-eq? (rember-f1 eq?))
(define rember-f2-equal? (rember-f1 equal?))

((rember-f1 eq?) 'a '(b a c))
(rember-f2-eq? 'a '(b a c))
(rember-f2-equal? 'a '(b a c))

((rember-f1 eq?) 'tuna '(shrimp and tuna salad))

((rember-f1 eq?) eq? '(equal? eq? eqan? eqlist? eqpair?))
