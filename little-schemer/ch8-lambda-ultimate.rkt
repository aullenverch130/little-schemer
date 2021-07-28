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

;; from ch. 3
(define insertL 
  (lambda (new old lat) 
    (cond 
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cons old (cdr lat))))
      (else (cons (car lat) (insertL new old (cdr lat)))))))
(insertL 'topping 'fudge '(ice cream with fudge for dessert))

;; from ch. 3
(define insertR
  (lambda (new old lat)
    (cond 
      ((null? lat) '())
      ((eq? (car lat) old)
        (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))
(insertR 'topping 'fudge '(ice cream with fudge for dessert))


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

(define insertL-f
 (lambda (test?)
  (lambda (new old lat)
    (cond 
        ((null? lat) '())
        ((test? (car lat) old) 
            (cons new (cons old (cdr lat))))
        (else (cons (car lat) ((insertL-f test?) new old (cdr lat))))))))
((insertL-f eq?) '2 'b '(a b c))

(define insertR-f
 (lambda (test?)
  (lambda (new old lat)
    (cond 
      ((null? lat) '())
      ((test? (car lat) old)
        (cons old (cons new (cdr lat))))
      (else (cons (car lat) ((insertR-f test?) new old (cdr lat))))))))
((insertR-f eq?) '2 'b '(a b c))

;; my attempt @ insert-g
(define insert-g
 (lambda (LorR)
  (lambda (test?)
   (lambda (new old lat)
     ((LorR test?) new old lat)))))
(((insert-g insertL-f) eq?) '2 'b '(a b c))


;; defining seq funcs
(define seqL 
    (lambda (new old l)
        (cons new (cons old l))))
(seqL 'a 'b 'c)
(define seqR
    (lambda (new old l)
        (cons old (cons new l))))
(seqR 'a 'b 'c)


;; interesting, here we are not returning the "insertL" procedure
;; but we are returning a function that resembles it by using
;; swapping the (cons new (cons old (cdr lat)))
;; and (cons old (cons new (cdr lat))) lines..
(define insert-g1
 (lambda (seq)
  (lambda (new old lat)
    (cond 
      ((null? lat) '())
      ((eq? (car lat) old)
        (seq new old (cdr lat)))
      (else (cons (car lat) 
        ((insert-g1 seq) new old (cdr lat))))))))
(print "insert-g1 test") (newline)
((insert-g1 seqR) 2 'b '(a b c))

;; making a short cut for writing the funcs
(define insertL1 (insert-g1 seqL))
(define insertR1 (insert-g1 seqR))
(insertL1 2 'b '(a b c))
(insertR1 2 'b '(a b c))

;; TODO: understand..
(define insertL2 
    (insert-g1
        (lambda (new old lat)
            (cons new (cons old lat)))))
(insertL2 2 'b '(a b c))


;; bringing back subst!
(define subst
    (lambda (new old l)
        (cond
            ((null? l) '())
            ((eq? (car l) old) 
                (cons new (cdr l)))
            (else (cons (car l) 
                    (subst new old (cdr l)))))))
;; define a func like seqL / seqR but for subst

(define seqsub
    (lambda (new old lat)
        (cons new lat)))
(seqsub 'a 'b '(1 2))
;; writing seqsub w/ subst
(define subst1 (insert-g1 seqsub))
(subst1 2 'b '(a b c))

;; example from book that is like rember
(define yyy
    (lambda (a l)
        ((insert-g1 seqrem) #f a l)))
(define seqrem 
    (lambda (new old l) l))
(yyy 'sausage '(pizza with sausage and bacon))
;; ^^ new is never used, so #f is never used.. 
;; anything could be in place of #f and it would work
;; but there must be something there for it to be 
;; compatable with seqL and SeqR..

;; book brings up value from ch. 6
