#lang racket

(define atom? 
  (lambda (x) 
    (and (not (pair? x)) (not (null? x)))))


(atom? 14) ;=> #t   
(atom? -14) ;=> #t   
(atom? 3.14) ;=> #t
;; ^^ what do they mean by "we don't consider"
;; or "in practice"

(define add1
  (lambda (x) 
    (+ x 1)))

(define sub1 
  (lambda (x)
    (- x 1)))

(sub1 -5) ;=> -6

(zero? 32874) ;=> #f   
;;(zero? '())
;;Error: zero?: number required, but got () [zero?]
(zero? 0) ;=> #t

(define myzero?
  (lambda (x) 
    (cond ((eq? 0 x) #t)
          (else #f))))
(myzero? 0) ;=> #t   
(myzero? '()) ;=> #f   
(myzero? '(sdkj fj sl)) ;=> #f   
(myzero? 9) ;=> #f
;; ^^ my zero? works with non numbers!

(+ 5 15) ;=> 20
;; the prefix language!!

(define myplus
  (lambda (n counter)
    (cond ((zero? counter) n)
          (else (myplus (add1 n) (sub1 counter))))))

; little schemer does myplus a little differently

(define mypower
  (lambda (base n counter)
    (cond ((zero? counter) ))))
          
(myplus 3 2)    
(myplus 5 1)
(myplus -5 10)
(myplus 2.5 5) ;=> 7.5
(myplus 0 0)

; (* 2 (* 2 (* 2 1)))
; (* 1 1)
; (+ 5 5)

(define mypoweri 
  (lambda (base exp result)
    (cond ((and (zero? exp) (eq? result base)) 1)
          ((zero? exp) result)
          (else (mypoweri base (sub1 exp) 
                            (* result base))))))

(myplus 2 3)

(define mysub 
  (lambda (n1 n2)
    (cond ((zero? n2) n1)
          (else (mysub (sub1 n1) (sub1 n2))))))

(mysub 5 4)
(mysub 4 5)
(mysub -5 4)
;(mysub -5 -5) this causes infinite recursion

;; this is the mysub from book
(define mysubook
  (lambda (n m)
    (cond 
      ((zero? m) n)
      (else (sub1 (mysubook n (sub1 m)))))))
;; the reason why he was saying non neg real numbers is 
;; becasue with this func you can't use neg or decimal numbers.. ;;; don't try it!!! 

;; my try @ addtup
(define addtup 
    (lambda (tup)
        (cond 
            ((null? tup) 0)
            (else (myplus (car tup) (addtup (cdr tup)))))))
;; interesting, your argument becomes the "type" of list
;; && then it's called  recursivly w/ cdr.. 
;; applying all the car of lists afterwards..

;; TODO: what if you made a * func that used tup?? "timestup"

;; my try at making a product func
(define myproduct 
    (lambda (n m)
        (cond 
            ((zero? m) 0)
            (else (myplus n (myproduct n (sub1 m)))))))

;; 12 x 3 = 12 + (12 x 2)
;;        = 12 + 12 + (12 x 1)
;;        = 12 + 12 + 12 + (x 12 0)
;;        = 12 + 12 + 12 + 0
;; ^^ can use w/ 
;; ^^ does this mean that the "terminal contition"
;; of a recursive function will always be the identity
;; element of the type of the function's argument??

;; tupplus
(define tupplus 
    (lambda (tup1 tup2)
        (cond 
            ((and (null? tup1) (null? tup2)) '())
            (else (cons (myplus (car tup1) (car tup2)) 
                    (tupplus (cdr tup1) (cdr tup2)))))))







(define map 
    '((o o o o o)
      (o o o o o)
      (o o o o o)
      (o o o o o)))
; (display "The result is ")
; (display (+ 3 4))
; (newline)
;; proj idea! create a "rouge like"
;; figure out how to print to screen!!

(define printmap
    (lambda (x)
        (print x)))

(define displaylist
    (lambda (x)
        (cond  
            ((null? x) (newline))
            (else 
                (newline)
                (display (car x)) 
                (displaylist (cdr x))))))
;; ^^ could use this to display lists for a "rouge like"

(display "Please enter your name: ")
(define name (read))
(display "Hello, ")
(display name)
(newline)
;; ex: on how to get input!!







;; tupplus
(define tupadd
    (lambda (tup1 tup2)
        (cond 
            ((null? tup2) tup1)
            ((null? tup1) tup2)
            ((or (null? tup2) (null? tup1)) )
            (else (cons (myplus (car tup1) (car tup2)) 
                    (tupadd (cdr tup1) (cdr tup2)))))))

(define my> 
    (lambda (m n)
        (cond 
            ((zero? m) #f) 
            ((zero? n) #t)
            (else (my> (sub1 m) (sub1 n))))))

(define my<
    (lambda (m n)
        (cond 
            ((zero? n) #f)
            ((zero? m) #t) 
            (else (my< (sub1 m) (sub1 n))))))

;; wrote before looking @ books ex:
(define my= 
    (lambda (m n)
        (cond
            ((and (zero? m) (zero? n)) #t)
            ((or (zero? m) (zero? n)) #f)
            (else (my= (sub1 m) (sub1 n))))))

(define my><
    (lambda (m n)
        (cond  
            ((and (not (my> m n)) (not (my< m n))) #t)
            (else #f))))
;; the books version
(define my<> 
    (lambda (n m)
        (cond 
            ((my> n m) #f)
            ((my< n m) #f)
            (else #t))))

;; my try @ exponent func
(define my^
    (lambda (n m)
        (cond 
            ((zero? m) 1)
            (else (myproduct n (my^ n (sub1 m)))))))

;; from book, introduced as ???
(define divideby
    (lambda (n m)
        (cond
            ((my< n m) 0)
            (else (add1 (divideby (mysubook n m) m))))))

;; my try @ a length func
(define len 
    (lambda (x)
        (cond 
            ((null? x) 0)
            (else (add1 (len (cdr x)))))))

;; my try @ a "pick" func
(define pick
    (lambda (n lat)
        (cond
            ((or (null? lat) (zero? n)) "no pick")
            ((eq? n 1) (car lat))
            (else (pick (sub1 n) (cdr lat))))))
            ;; think "sub1 of n each recurrence"
            ;; think use len
(pick 4 '(a b c d)) ;=> 'd
(pick 3 '(a b c d)) ;=> 'c

;; my try @ remove a pick func
(define repick
    (lambda (n lat)
        (cond 
            ((null? lat) '())
            ((eq? n 1) (repick (sub1 n) (cdr lat)))
            (else (cons (car lat) (repick (sub1 n) (cdr lat)))))))
(repick 0 '(a)) ;=> '(a)
(repick 3 '(a b c d)) ;=> '(a b d)
(repick 1 '(a b c d)) ;=> '(b c d)
(repick 4 '(a b c d)) ;=> '(a b c)
;; ^^ I did it differentlly 

(define rempick
    (lambda (n lat)
        (cond
            ((zero? (sub1 n)) (cdr lat))
            (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
;; ^^ I think my terminal condition works better!?
;; it allows for the pick to be 0..

;; my try @ a func that returns a list w/ no numbers
(define nonum
    (lambda (lat)
        (cond
            ((null? lat) '())
            ((number? (car lat)) (nonum (cdr lat)))
            (else (cons (car lat) (nonum (cdr lat)))))))
(nonum '(5 pears 6 prunes 9 dates)) ;=> '(pears prunes dates)

;; my try @ func that takes lat and returns tup
(define allnum
    (lambda (lat)
        (cond
            ((null? lat) '())
            ((number? (car lat)) 
                (cons (car lat) (allnum (cdr lat))))
            (else (allnum (cdr lat))))))
(allnum '(5 pears 6 prunes 9 dates)) ;=> '(5 6 9)

;; eqan? from book 
(define eqan? 
    (lambda (a1 a2)
        (cond
            ((and (number? a1) (number? a2)) (my= a1 a2))
            ((or (number? a1) (number? a2)) #f)
            (else (eq? a1 a2)))))
;; I didn't do this func, nor did I get the point?

;; my try @ occur
(define occur?
    (lambda (a lat)
        (cond
            ((null? lat) 0)
            ((eq? a (car lat)) (add1 (occur? a (cdr lat))))
            (else (occur? a (cdr lat))))))
(occur? 'a '(a a a b a c a d e)) ;=> 5

;; my try at one? func
(define one?
    (lambda (n)
        (cond
            ((and (my> n 0) (my< n 2)) #t)
            (else #f))))
;; my try @ a simplified version..
(define one?? (lambda (n) (my= n 1)))

;; rewriting rempick w/ one??
(define rempick2
    (lambda (n lat)
        (cond
            ((null? lat) '())
            ((one?? n) (cdr lat))
            (else (cons (car lat) 
                    (rempick2 (sub1 n) (cdr lat)))))))
(rempick2 2 '(a b c d)) ;=> '(a c d)

;; TODO: use cons to create a tree
;; maybe take a tup and output (# "# of times it occurs")
;; (1 3 1 2 1 1 3) => ((1 4) (2 1) (3 2))
;; (1 3 1 (3 3 3 2 2 1) 2 1 1 3) => ( (1 4) (2 1) (3 2) 
;;                                   ((1 1) (2 2) (3 3)) )
;; or takes tup and outputs nested ordered
;; ex: (1 1 4 3 3 6 5) => (((((1 1) 3 3 ) 4) 5) 6) 
;; ^^ maybe a version that takes into account how deep it is?
;; ^^ maybe the * OH MY GAWD ch would help w/ this!?

;; test name

