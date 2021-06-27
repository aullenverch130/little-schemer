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
'((b) (s ((((b i))) (c (b)) s) ) (b)(br)(b br) )

;; attempts at writting out the evaluation for ^^
; (subst* 'o 'b '((b) (s ((((b i))) (c (b)) s) ) (b)(br)(b br) ))
; (cons 
;   (subst* 'o 'b '(b))
;   (subst* 'o 'b '((s ((((b i))) (c (b)) s) ) (b)(br)(b br) )))
; (cons 
;   (cons 'o (subst* 'o 'b '()))
;   (subst* 'o 'b '((s ((((b i))) (c (b)) s) ) (b)(br)(b br) )))
; (cons 
;   (cons 'o '())
;   (subst* 'o 'b '((s ((((b i))) (c (b)) s) ) (b)(br)(b br) )))
; (cons 'o
;   (subst* 'o 'b '((s ((((b i))) (c (b)) s) ) (b)(br)(b br) )))
; (cons 'o
;   (cons (subst* 'o 'b '(s ((((b i))) (c (b)) s) ) ))
;   (subst* 'o 'b '((b)(br)(b br))))
; (cons 'o
;   (cons (cons 's (subst* 'o 'b '((((b i))) (c (b)) s)) ))
;   (subst* 'o 'b '((b)(br)(b br))))
; (cons 'o
;   (cons 
;     (cons 's 
;       (cons (subst* 'o 'b '((((b i)))) )
;             (subst* 'o 'b '((c (b)) s) ))
;   (subst* 'o 'b '((b)(br)(b br))))
; (cons 'o
;   (cons 
;     (cons 's 
;       (cons (subst* 'o 'b '((((b i)))) )
;       (subst* 'o 'b '((c (b)) s) ))
  ; (subst* 'o 'b '((b)(br)(b br))))

;; my try @ insertL*
(define insertL* 
  (lambda (new old l)
    (cond 
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? old (car l)) 
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
             (eqlist? (cdr l1) (cdr l2))))
      ((atom? (car l1)) #f) ;; atom list
      ((null? l2) #f) ;; list empty 
      ((atom? (car l2)) #f) ;; list atom
      (else ;; list list
        (and (eqlist1? (car l1) (car l2))
             (eqlist1? (cdr l1) (cdr l2)))))))
; (eqlist1? '(strawberry ice cream) '(strawberry ice cream))
; (eqlist1? '(banana ((split))) '((banana) (split)))




;; equal list w/ parens 
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t) ;<-- it got to the end
      ((or (null? l1) (null? l2)) #f)

      ;; so close 
      ((and (atom? (car l1)) (atom? (car l2)))
        (and (eq? (car l1) (car l2))
             (eqlist? (cdr l1) (cdr l2))))
      
      ((or (atom? (car l1)) (atom? (car l2))) #f)

      (else (and (eqlist? (car l1) (car l2)) 
                 (eqlist? (cdr l1) (cdr l2)))))))
; (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda))))
; (eqlist? '(strawberry ice cream) '(strawberry ice cream))
; (eqlist? '(banana ((split))) '((banana) (split)))






 
