#lang racket

; atom? from book
(define atom? 
  (lambda (x) 
    (and (not (pair? x)) (not (null? x)))))

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
(eqlist? '(b ((s)) (a (o))) '(b ((s)) (a (o))))






;; my try @ numbered?
(define numbered? 
    (lambda (aexp)
        (cond 
            ((atom? aexp) 
                (number? aexp))
            ((eq? (car (cdr aexp)) '+ )
                (and (number? (car aexp)) 
                     (number? (car (cdr (cdr aexp))))))
            ((eq? (car (cdr aexp)) 'x )
                (and (number? (car aexp)) 
                     (number? (car (cdr (cdr aexp))))))
            ((eq? (car (cdr aexp)) '^ )
                (and (number? (car aexp)) 
                     (number? (car (cdr (cdr aexp)))))))))
(numbered? '(2 + (2 ^ 2)))
(numbered? '(1 + 2))
;; number OR (number operator number)

;; simplification of numbered?
(define numbered1?
    (lambda (aexp)
        (cond 
            ((atom? aexp) (number? aexp))
            (else 
                (and (numbered1? (car aexp))
                     (numbered1? (car (cdr (cdr aexp)))))))))

;; my very iterative ^ func..
(define my^ 
    (lambda (val base exp)
        (cond
            ((zero? (sub1 exp)) val)
            (else (my^ (* base val) base (sub1 exp))))))

;; attempt at writing value
(define value 
    (lambda (nexp)
        (cond
            ((atom? nexp) nexp)
            ((eq? (car (cdr nexp)) '+) 
                (+ (car nexp) (value (car (cdr (cdr nexp))))))
            ((eq? (car (cdr nexp))'x) 
                (* (car nexp) (value (car (cdr (cdr nexp))))))
            (else 
                (my^ (car nexp) (car nexp) 
                        (value (car (cdr (cdr nexp)))))))))
(value '(1 + (3 ^ 4)))



(define value1
    (lambda (nexp)
        (cond
            ((atom? nexp) nexp)
            ((eq? (car nexp) '+)
                (+ (value1 (car (cdr nexp))) 
                   (value1 (car (cdr (cdr nexp))))))
            ((eq? (car nexp) 'x)
                (* (value1 (car (cdr nexp)))
                   (value1 (car (cdr (cdr nexp))))))
            (else 
                (my^ (value1 (car (cdr nexp))) 
                     (value1 (car (cdr (cdr nexp)))))))))
(value1 '(+ 1 (x 3 4)))


;; trying sub-aexp1
(define subexp1
    (lambda (aexp)
        (car aexp)))
(define subexp2
    (lambda (aexp)
        (car (cdr (cdr aexp)))))
;; operator
(define operator
    (lambda (aexp)
        (car (cdr aexp))))

;; writing value again!
(define value2
    (lambda (aexp)
        (cond
            ((atom? aexp) aexp)
            ((eq? (operator aexp) '+)
                (+ (value2 (subexp1 aexp))
                   (value2 (subexp2 aexp))))
            ((eq? (operator aexp) 'x)
                (* (value2 (subexp1 aexp))
                   (value2 (subexp2 aexp))))
            (else (my^ (value2 (subexp1 aexp)) 
                       (value2 (subexp2 aexp)))))))
(value2 '(1 + (3 x 4)))

;; writing rep for numbers
;; () = 0, (()) = 1, (()()) = 2
(define iszero? 
    (lambda (num)
        (cond
            ((null? num) #t)
            (else #f))))
(define sero? 
    (lambda (num)
       (null? num)))
;; try at edd1.. (add1)
(define edd1
    (lambda (exp)
        (cond
            ((sero? exp) '(()))
            (else (cons '() (cons (car exp) (cdr exp)))))))
;; try zub1.. (sub1)
(define zub1
    (lambda (exp)
        (cond
            ((sero? exp) '())
            (else (cdr exp)))))
;; what the books wrote
(define edd1-
    (lambda (n)
        (cons '() n)))
(define zub1-
    (lambda (n)
        (cdr n)))


;; my try at plus w/ this "shadow" representation
(define my+
    (lambda (n m)
        (cond
            ((sero? m) n)
            (else (my+ (edd1- n) (zub1- m))))))
            ;; ^^ book does this by adding a edd1 every recursive call
;; book plus
(define book+
    (lambda (n m)
        (cond 
            ((sero? m) n)
            (else (edd1- (book+ n (zub1- m)))))))

