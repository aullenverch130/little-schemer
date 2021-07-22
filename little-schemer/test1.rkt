#lang racket


;; from ch. 2
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(define add1
  (lambda (x) 
    (+ x 1)))

;; set? 
(define set?
    (lambda (lat)
        (cond
            ((null? lat) #t)
            ((member? (car lat) (cdr lat)) #f)
            (else (set? (cdr lat))))))

;; try subset?
(define subset?
    (lambda (set1 set2)
        (cond
            ((null? set1) #t)
            ((member? (car set1) set2) 
                (subset? (cdr set1) set2))
            (else #f))))
(subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings))

;; firsts from ch. 3
(define firsts (lambda (x)
    (cond
      ((null? x) '())
      (else (cons (car (car x)) (firsts (cdr x)))))))
(firsts '((a b) (b c) (c d))) ;=> (a b c)   

;; seconds helper func
(define seconds (lambda (x)
    (cond
      ((null? x) '())
      (else (cons (car (cdr (car x))) (seconds (cdr x)))))))
(seconds '((a b) (b c) (c d))) ;=> (b c d)   


;; trying 1 cond line eqset
(define eqset?
    (lambda (set1 set2)
        (and (subset? set1 set2)
             (subset? set2 set1))))
(eqset? '(6 large chickens with wings) '(6 chickens with large wings))

;; writing "fun?", funcs can't have one-to-many rel


(define len 
    (lambda (x)
        (cond 
            ((null? x) 0)
            (else (add1 (len (cdr x)))))))

(define eqlen
  (lambda (a b)
    (eq? (len a) (len b))))


(eqlen '(1 2 3) '(4 5 6))

(define fun? 
    (lambda (domain codomain image) 
        (cond 
          ((not (set? domain)) #f)
          ((eqlen domain image) #t)
          (else #f))))
(fun? '(1 2 3 4 5) '(a b c d e) '(a b c d c))



;; TODO: domain needs to be set.., 

; (fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
; (fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))


;; define injectfun?
; (define injectfun?
;     (lambda (rel)
;         (and (fun? rel)
;              (set? (seconds rel)))))
; (print "faithful fun tests") (newline)
; (injectfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
; (injectfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))

; ;; surjectfun?
; (define surjectfun?
;   (lambda (rel codamin)
;     (and (fun? rel)
;              (set? (seconds rel)))))
;              ;; eqset? ^^
            

;(surjectfun? '((8 3)(4 2)))


;; TODO: bijectfun?