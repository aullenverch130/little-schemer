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

;; TODO: understand better..
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





;; book brings up value from ch. 6 and related functions
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
;; from ch. 4
(define my^
    (lambda (n m)
        (cond 
            ((zero? m) 1)
            (else (* n (my^ n (sub1 m)))))))

;; writing func that returns procedure based of atom given
(define atom->fun 
    (lambda (x)
        (cond
            ((eq? x '+) +)
            ((eq? x 'x) *)
            (else my^))))
    
;; the book is assuming that operator is (car aexp), aka prefix notation
;; (atom->fun (operator '(+ 5 3)))
(atom->fun (operator '(5 + 3)))

;; writing value with two cond lines
(define value3
    (lambda (aexp)
        (cond
            ((atom? aexp) aexp)
            (else ((atom->fun (operator aexp))
                        (value3 (subexp1 aexp))
                        (value3 (subexp2 aexp)))))))
(value3 '(1 + (3 x 4)))

;; book brings back multirember
(define multirember
    (lambda (a lat)
        (cond
            ((null? lat) '())
            ((eq? (car lat) a)
                (multirember a (cdr lat)))
            (else (cons (car lat)
                    (multirember a (cdr lat)))))))
;; writing multirember-f for other eq?
(define multirember-f
 (lambda (test?)
  (lambda (a lat)
    (cond
        ((null? lat) '())
        ((test? (car lat) a)
            ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat) 
                ((multirember-f test?) a (cdr lat))))))))
((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))

;; RECURSIVE CURRYING!!?!

;; church encodings!
((lambda (x) x) 'y) ;; id 
(((lambda (a) (lambda (b) a)) 'x) 'y) ;; true
(((lambda (a) (lambda (b) b)) 'x) 'y) ;; false

(define multirember-eq? (multirember-f eq?))
(define multirember-equal? (multirember-f equal?))
multirember-eq?
multirember-equal?
(multirember-eq? 'tuna '(shrimp salad tuna salad and tuna))
(multirember-equal? 'tuna '(shrimp salad tuna salad and tuna))


(define eq?-tuna (eq?-c 'tuna))
;; eq?-salad is defined up there ^^

(define multirember-T
    (lambda (test? lat)
        (cond
            ((null? lat) '())
            ((test? (car lat)) 
                (multirember-T test? (cdr lat)))
            (else 
                (cons (car lat) (multirember-T test? (cdr lat)))))))
; (multirember-T eq?-salad '(shrimp salad tuna salad and tuna))
(multirember-T eq?-tuna '(shrimp salad tuna salad and tuna))
(multirember-T eq?-salad '(shrimp salad tuna salad and tuna))


;; multirmeber func from book w/ collectors (continuations)
(define multirember&co
 (lambda (a lat col)
  (cond
    ((null? lat) 
        (col '() '()))
    ((eq? (car lat) a) 
        (multirember&co a
            (cdr lat)
            (lambda (newlat seen)
                (col newlat (cons (car lat) seen)))))
    (else
        (multirember&co a 
            (cdr lat)
            (lambda (newlat seen)
                (col (cons (car lat) newlat) seen)) )))))
;; collectors
(define a-friend ;; #t if second is null..
    (lambda (x y)
        (null? y)))
; (define new-friend
;     (lambda (newlat seen)
;         (col newlat
;             (cons (car lat) seen))))
(define len ;; from ch.4
    (lambda (x)
        (cond 
            ((null? x) 0)
            (else (add1 (len (cdr x)))))))
(define last-friend
    (lambda (x y)
        (len y)))

(multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)
(multirember&co 'tuna '() a-friend)
(multirember&co 'tuna '(tuna) a-friend)
(multirember&co 'tuna '(strawberries tuna and swordfish) last-friend)



;; attempt @ multiinsertLR
(define multiinsertLR
 (lambda (new oldL oldR lat)
  (cond
    ((null? lat) '())
    ((eq? oldL (car lat))
        (cons new (cons oldL 
            (multiinsertLR new oldL oldR (cdr lat)))))
    ((eq? oldR (car lat))
        (cons oldR (cons new 
            (multiinsertLR new oldL oldR (cdr lat)))) )
    (else 
        (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))
(multiinsertLR '@ 'b 'c '(a b c d b))
;; ^^ you could also use seqL & seR, but w/o much benifit


;; multiinsertLR&co from book
(define multiinsertLR&co
 (lambda (new oldL oldR lat col)
  (cond
    ((null? lat) (col '() 0 0))
    ((eq? oldL (car lat))
        (multiinsertLR&co new oldL oldR (cdr lat) 
            (lambda (newlat L R)
                (col (cons new (cons oldL newlat))
                     (add1 L) R))))
    ((eq? oldR (car lat))
        (multiinsertLR&co new oldL oldR (cdr lat) 
            (lambda (newlat L R)
                (col (cons oldR (cons new newlat))
                   R (add1 L) ))))
    (else 
        (multiinsertLR&co new oldL oldR (cdr lat)
            (lambda (newlat L R)
                (col (cons (car lat) newlat) L R)))))))
; (multiinsertLR&co '@ 'b 'c '(a b c d b) a-friend)
; (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) a-friend)

;; divide from ch. 4
(define divideby
    (lambda (n m)
        (cond
         ((< n m) 0)
         (else (add1 (divideby (- n m) m))))))


;; from book to solve evens-only*
(define even? 
    (lambda (n)
        (= (* (divideby n 2) 2) n)))

;; writing evens-only* 
(define evens-only*
 (lambda (lat)
  (cond
    ((null? lat) '())
    ((atom? (car lat)) 
        (cond ((even? (car lat)) 
                (cons (car lat) (evens-only* (cdr lat))))
              (else (evens-only* (cdr lat)))))
    (else (cons 
            (evens-only* (car lat))
            (evens-only* (cdr lat)))))))
(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))


(define evens-only*&co
 (lambda (l col)
  (cond 
    ((null? l) 
        (col '() 1 0))
    ((atom? (car l))
      (cond
        ((even? (car l))
            (evens-only*&co (cdr l)
                (lambda (newl p s)
                    (col (cons (car l) newl)
                         (* (car l) p) s))))
        (else (evens-only*&co (car l)
                (lambda (newl p s)
                    (col newl p (+ (car l) s)))))) )
    (else (evens-only*&co (car l)
            (lambda (al ap as)
                (evens-only*&co (car l)
                    (lambda (dl dp ds)
                        (col (cons al dl)
                             (* ap dp)
                             (+ as ds))))))))))
;; collectors for evens-only*&co
(define the-last-friend
    (lambda (newl product sum)
        (cons sum
            (cons product newl))))
; (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)
;; TODO: this func ^^ doesn't work.. and I don't understand it..


;; Ch. 8 = 22pg.
;; Ch. 9 = 25pg.






;; messing around with currying!!
(define concat-c3
    (lambda (fst)
        (lambda (snd)
          (lambda (thrd)
              (string-append fst snd thrd) ))))

(define string-append-c
    (lambda (fst)  
        (lambda (snd)
           (list->string 
              (append (string->list fst)    
                      (string->list snd))))))
; (((concat-c3 "testing") " ") "concat-c3")
; ((string-append-c "testing ") "string-append-c")


;;; TODO: cons it on to the tail.. backwards.. 
(define appendlist
  (lambda (l1 l2) 
      (cond
        ((null? l2) l1)
        (else (appendlist (cons (car l2) l1) (cdr l2))))))

(appendlist '(1 2 3) '(4 5))





; (plus-c )
; (times-c )





;; LAMBDA CALC EXP
((lambda (x) x) 'y) ;; id 
(((lambda (a) (lambda (b) a)) 'x) 'y) ;; true
(((lambda (a) (lambda (b) b)) 'x) 'y) ;; false


(define true 
   (lambda (a) 
     (lambda (b) 
             a)))

(define false
  (lambda (a) 
    (lambda (b) 
            b)))

(define id
  (lambda (x)
            x))

((true 'x) 'y)
((false 'x) 'y)
(id 'y)

;; example of lambda calc syntax
;; (((La.Lb.a) x) y) => x
;; La.Lb.b





; "pass by value" vs "pass by reference"
;                 C++ gives option..