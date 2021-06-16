
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
;;(mysub -5 -5) <-- stack overflow!