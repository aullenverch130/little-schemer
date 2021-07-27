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
;; my= from book ch4
(define my= 
  (lambda (n m)
    (cond 
      ((zero? m) (zero? n))
      ((zero? n) #f)
      (else (my= (sub1 n) (sub1 m))))))
      ;; ^^ the cases are acounted for if either m or n hit zero
;; eqan? from book, ch 4
(define eqan? 
    (lambda (a1 a2)
        (cond
            ((and (number? a1) (number? a2)) (my= a1 a2))
            ((or (number? a1) (number? a2)) #f)
            (else (eq? a1 a2)))))
;; doesn't work with none symbol atoms
(define stringify*
  (lambda (l)
    (cond
      ((null? l) "")
      ((atom? (car l))
        (cond 
          ((null? (cdr l)) 
            (string-append 
                (symbol->string (car l)) (stringify* (cdr l))))
          (else 
            (string-append 
                (symbol->string (car l)) " " (stringify* (cdr l))))))
      (else 
        (cond 
          ((null? (cdr l)) 
            (string-append "("
              (stringify* (car l)) ")" (stringify* (cdr l))))
          (else 
            (string-append "("
              (stringify* (car l)) ") " (stringify* (cdr l)))))))))
(define print-S
  (lambda (s)
    (display (string-append "(" s ")"))))
(print-S (stringify* '(a (b) c))) ;=> (a (b) c)
(print-S (stringify* '(a ((b)) c))) ;=> (a ((b)) c)


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

;; my try @ occur*
(define occur* 
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l)) 
        (cond 
          ((eq? a (car l)) (add1 (occur* a (cdr l))))
          (else (occur* a (cdr l)))))
      (else (myplus (occur* a (car l))
                  (occur* a (cdr l)))))))
'(a (a b) a)

;; writing out the order I think it evaluates
(occur* 'a '(a (a b) a) )
(add1 (occur* 'a '((a b) a)))
(add1 (myplus (occur* 'a '(a b)) (occur* 'a '(a))))
(add1 (myplus (add1 (occur* 'a '(b))) (occur* 'a '(a))))
(add1 (myplus (add1 (occur* 'a '())) (occur* 'a '(a))))
(add1 (myplus (add1 0) (occur* 'a '(a))))
(add1 (myplus (add1 0) (add1 (occur* 'a '()))))  ;; <-- or does it eval (add1 0)
(add1 (myplus (add1 0) (add1 0))) 
(add1 (myplus 1 (add1 0)))
(add1 (myplus 1 1))
(add1 2)
3
; ^^ how does it know to finally eval once it has the correct args for the func call?



;; my try at subst* func
(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? old (car l)) 
            (cons new (subst* new old (cdr l))))
          (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons 
              (subst* new old (car l))
              (subst* new old (cdr l)))))))
'((banana)
  (split ((((banana ice)))
          (cream (banana))
          sherbet))
  (banana)
  (bread)
  (banana brandy))



;; my try @ insertL*
(define insertL* 
  (lambda (new old l)
    (cond 
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((equal1? old (car l)) 
            (cons new (cons (car l) (insertL* new old (cdr l)))))
          (else 
            (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l))
                  (insertL* new old (cdr l)))))))
'((how much (wood))
  could
  ((a (wood) chuck))
  (((chuck)))
  (if (a) ((wood chuck)))
  could cuck wood)






  ;; my try @ member*
(define member*
  (lambda (a l)
    (cond 
      ((null? l) #f)
      ((atom? (car l)) 
        (cond
          ((eq? a (car l)) #t) (else (member* a (cdr l)))))
      (else (or
              (member* a (car l)) (member* a (cdr l)))))))
'((potato) (chips ((with) fish) (chips)))
;; ^^ book did it different


;; version of member* from book
(define member2*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
        (or (eq? a (car l)) (member2* a (cdr l))))
      (else (or (member2* a (car l)) (member2* a (cdr l)))))))

;; my try @ leftmost
(define leftmost 
  (lambda (l)
    (cond
      ((null? l) "no leftmost atom")
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))
(leftmost '(((() four)) 17 (seventeen))) ;=> "no leftmost atom"
(leftmost '(((hot) (tuna (and))) cheese)) ;=> 'hot

;; ex: of and returning #t
(and (atom? (car '(a b c))) (eq? (car '(a b c)) 'a))

;; the 9 cases
; empty empty, atom atom, list list
; empty list, empty atom
; atom empty, atom list 
; list empt, list atom

(define eqlist1?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t) ;; empty empty
      ((and (null? l1) (atom? (car l2))) #f) ;; empty atom
      ((null? l1) #f) ;; empty list
      ((and (atom? (car l1)) (null? l2)) #f) ;; atom empty
      ((and (atom? (car l1)) (atom? (car l2))) ;; atom atom
        (and (eq? (car l1) (car l2))
             (eqlist1? (cdr l1) (cdr l2))))
      ((atom? (car l1)) #f) ;; atom list
      ((null? l2) #f) ;; list empty 
      ((atom? (car l2)) #f) ;; list atom
      (else ;; list list
        (and (eqlist1? (car l1) (car l2))
             (eqlist1? (cdr l1) (cdr l2)))))))
; (eqlist1? '(strawberry ice cream) '(strawberry ice cream))
; (eqlist1? '(banana ((split))) '((banana) (split)))

(define eqlist2?
  (lambda (lg l1 l2)
    (cond
      ((and (null? l1) (null? l2)) (display lg) #t) ;<-- it got to the end
      ((or (null? l1) (null? l2)) (display lg) #f)

      ;; so close 
      ((and (atom? (car l1)) (atom? (car l2)))
        (and (eq? (car l1) (car l2))
             (eqlist2? lg (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) (display lg) #f)

      (else 
        (and (eqlist2? lg (car l1) (car l2)) 
                 (eqlist2? lg (cdr l1) (cdr l2)))))))
; (eqlist2? '() '(b ((s)) (a (o))) '(b ((s)) (a (o))))
 


;; equal list w/ parens 
(define eqlistl?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t) ;<-- it got to the end
      ((or (null? l1) (null? l2)) #f)

      ((and (atom? (car l1)) (atom? (car l2)))
        (and (eq? (car l1) (car l2))
             (eqlist? (cdr l1) (cdr l2))))
      
      ((or (atom? (car l1)) (atom? (car l2))) #f)

      (else (and (eqlist? (car l1) (car l2)) 
                 (eqlist? (cdr l1) (cdr l2)))))))
; (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda))))
; (eqlist2? '(strawberry ice cream) '(strawberry ice cream))
; (eqlist2? '(banana ((split))) '((banana) (split)))
; (eqlist2? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))

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
; (eqlist?log '() '(b ((s)) (a (o))) '(b ((s)) (a (o))))
 
 ;; TODO: plan, after everything has evaluated, display everything

(define equal1? 
  (lambda (s1 s2)
    (cond 
      ((and (atom? s1) (atom? s2))
        (eq? s1 s2))
      ((atom? s1) #f)
      ((atom? s2) #f)
      (else 
        (eqlist? s1 s2)))))

; (display "(and (equal? ") (display (car l1)) (display " ") (print (car l2)) (display ") ") 
;         (display "(equal? ") (print (cdr l1)) (display " ") (print (cdr l2)) (display "))") 
;         (newline)

;; there is no need for the atom? vs else conditions!!
(define rember
  (lambda (s l)
    (cond 
      ((null? l) '())
      ((atom? (car l))
        (cond 
          ((equal1? (car l) s) (cdr l))
          (else (cons (car l) (rember s (cdr l))))))
      (else
        (cond 
          ((equal1? (car l) s) (cdr l))
          (else (cons (car l) (rember s (cdr l)))))))))
(rember '(a) '((b) (a) c)) ;=> '((b) c)

(define rembersim
  (lambda (s l)
    (cond 
      ((null? l) '())
      (else
        (cond 
          ((equal1? (car l) s) (cdr l))
          (else (cons (car l) (rembersim s (cdr l)))))))))
(rembersim '(a) '((b) (a) c)) ;=> '((b) c)

(define rembers2
  (lambda (s l)
    (cond 
      ((null? l) '())
      (else
        (cond 
          ((equal1? (car l) s) (cdr l))
          (else (cons (car l) (rembers2 s (cdr l)))))))))
(rembers2 '(a) '((b) (a) c)) ;=> '((b) c)

;; same as insertL* but inserts S exp too
(define SinsertL* 
  (lambda (new old l)
    (cond 
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? old (car l)) 
            (cons new (cons (car l) (SinsertL* new old (cdr l)))))
          (else 
            (cons (car l) (SinsertL* new old (cdr l))))))
      ((equal1? old (car l))
        (cons new (cons (car l) (SinsertL* new old (cdr l)))))
      (else (cons (SinsertL* new old (car l))
                  (SinsertL* new old (cdr l)))))))
(SinsertL* '(c) '(b) '((b) b (a) c (b (b))))