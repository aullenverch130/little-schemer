#lang racket

(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))
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


;; ?? why is this #t
(list? '(how (are))) ;=> #t
(list? '(8 7 8)) ;=> #t
;; maybe list? just looks for () as an S-exp within (list?   )

;; this is null list
(list? '()) ;=> #t
(list? (8)) ;=> #t

;; diff between S-exp / atom / list ???
  ;; S-exp = atom or list
  ;; atom = any S-exp that has '  but no () 
  ;; list = S-exp w/ () 
(atom? '()) ;=> #f

;; car tests, returns 1st
(car '(a b c)) ;=> a


;; why!??!?
(cons 'e 'd) ;=> (e . d)
(cons '() '()) ;=> (())



