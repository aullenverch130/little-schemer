#lang racket

(define atom? 
  (lambda (x) 
    (and (not (pair? x)) (not (null? x)))))
;; (define car? (lambda (x) ()))

;; testing
'(x y z)
(atom? '(x y s))
(list? '(x d j))
;; testing w/ repl
(list? '(how are you doing today)) 
;=> #t   
(atom? 'how)
;=> #t

;; test
;; test 2

;; ?? why is this #t
(list? '(how (are))) ;=> #t
(list? '(8 7 8)) ;=> #t
;; maybe list? just looks for () as an S-exp within (list?   )

;; this is null list
(list? '()) ;=> #t
(list? '(8)) ;=> #t

;; diff between S-exp / atom / list ???
  ;; S-exp = atom or list
  ;; atom = any S-exp that has '  but no () 
  ;; list = S-exp w/ () 
(atom? '()) ;=> #f

;; car tests, returns 1st
(car '(a b c)) ;=> a
(car '((a b c) x y z)) ;=> (a b c)


;; why!??!?
(cons 'e 5) ;=> (e . 5)
(cons 5 '5) ;=> (e . 5)
(cons 5 5) ;=> (5 . 5)
(cons 'e 'd) ;=> (e . d)
(cons '() '()) ;=> (())
(cons '() '()) ;=> (())
(cons '() '()) ;=> (())
;; (cons e d) && (cons e ()) don't work!!
;; (cons e 'd), (cons (e) (d))
atom? '5 ;=> 5
;; wtf ^^^

;; why does car take a list w/ 2 s-exp!?
(car '(hotdog)) ;=> hotdog
(car '((hotdog))) ;=> (hotdog)
(car (car '(((hotdogs)) (and)))) ;=> (hotdogs)
;; ^^^ it's becasue all lists are a cons of
;; first && rest, car && cdr, when there is one 
;; element the rest or cdr is null or empty list


;; 1st cdr ex:
(cdr '(a b c)) ;=> (b c)   


;; why??
atom? ;=> #<Closure>


;; cdr of list w/ 1 atom = ()
(cdr '(hamburger)); => ()

;; note how one more () got around the c
(cdr (cdr '((b) (x y) ((c))))) ;=> (((c)))
;; no awnser becasue car '(a (b (c)) d) is an atom

(cons 'peanut '(butter and jelly)) 
;=> (peanut butter and jelly)
;; note it's not (peanut (butter and jelly))
;; cons adds s-exp (list or atom) to front of list

(cons '(banana and) '(peanut butter and jelly))
;=> ((banana and) peanut butter and jelly)
;; why does (cons () ()) ;=> (())
;; but ^^ must have quotes?? is it because they
;; are empty lists??

;; cons takes any s-exp && any list

;; the book says no awnser but in practice that it would work... there are those . again!!
(cons '((a b c)) 'b) ;=> (((a b c)) . b)

;; quotes stop evaluation??
;; maybe quotes must be used when you imbed lists?
'() ;=> ()  
'() '() ;=> ()  not sure why...
'(()) ;=> error... nil is not a function..
;; ^^ so if you imbed lists it expects a func?
;; also look above ^^ if you have non empty
;; paren or sybols it expects quotes

;; book says no awnser.. but I get (a . b)..
(cons 'a 'b) ;=> (a . b)
(cons 4 5) ;=> (4 . 5)

;; could the . syntax, ex: a . b be 
;; "a after b", it makes sense as a composition
(cons 'a (cons 'b '())) ;=> (a b)
;; which is diff than (a . b) but maybe this is
;; for a mo'e general syntax if you could cons 'b, 
;; a atom, (it should be a list) to 'a.. 

;; is the list empty? yes it is..
(null? '()) ;=> #t

;; the book says no awnser but it return #f
;; for everything not a list
(null? 'spegett) ;=> #f
;; the law of null? defined only for lists..

;; again when should we quote??
(define lat (lambda (x) x))
;; it seems quotes are only excepted on the 2nd
;; argument of lambda.. 
(lat 'd) ;=> #("halt")
;; ^^ if lambda has atom for 1st arg


;; attempt to get cond working w/ lambda define
;; (define lat (lambda (x) 
;;        (cond #t 'd 'w)))


;; used for first recursive example
(define lat? 
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
;; ^^ need to understand what args conds takes
;; (cond (if then) (if then) (else then) )
;; ^^  how does  ((null? l) #t) eval??
;; ^^ this has to do with how cond is defined
;; cond evaluates the next (if then) if 
;; (null? l) => #f, else the lat? func evals #t
(lat? '(() f d s)) ;=> #f
;; why!!!?


;; (cond ...) asks questions
;; (lambda ...) creates a function
;; (define ...) gives it a name

;; ex: of cond, just ("if" "then"), car of each
;; argument must return #t OR #f
(cond (#f 'a)(#f 'b)(#t 'c)) ; => c 
(cond (#f 'a)(#f 'b)(#f 'c)(#f 'd)(else 'f)) ;=> f


;; used for a group of Qs 
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))
;; ^^ allways test null 1st.. 
;; aka, end of the list, if cdr-ing the list every recur
;; like iterating through the s-exp of the list..

;; returns 1st arg if #t or any s-exp
;; if 1st arg is #f, return 2nd arg
(or #t 'a) ;=> #t
(or 'a 'b) ;=> a   
(or 'b 'c) ;=> b   
(or '(d s) 'c) ;=> (d s)
(or #f 'a) ;=> a
;; ^^ returns 2nd because 1st arg is #f