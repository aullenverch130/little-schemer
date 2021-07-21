#lang racket 

;; from book
(define atom? 
  (lambda (x) 
    (and (not (pair? x)) (not (null? x)))))
(define add1
  (lambda (x) 
    (+ x 1)))

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







(define stringify-ln*
  (lambda (depth l)
    ;; ln* creates a string of the database with new lines every row
    ;; depth is always zero and increase through nested lists
    (cond
      ((null? l) "")

      ((symbol? (car l))
        (cond 
          ((and (null? (cdr l)) (eq? (car l) '_)) 
            (string-append 
                " " (stringify-ln* depth (cdr l)))) 
          ((null? (cdr l)) 
            (string-append 
                (symbol->string (car l)) (stringify-ln* depth (cdr l))))
          (else 
            (string-append 
                (symbol->string (car l)) " " (stringify-ln* depth (cdr l))))))
 
      ((number? (car l))
        (cond 
          ((null? (cdr l)) 
            (string-append 
                (number->string (car l)) (stringify-ln* depth (cdr l))))
          (else 
            (string-append 
                (number->string (car l)) " " (stringify-ln* depth (cdr l))))))
      
      (else  
        (cond 
            ((eq? depth 0) 
                (string-append "\n"
                    (stringify-ln* (add1 depth) (car l)) "" 
                    (stringify-ln* depth (cdr l))))
            (else 
                (string-append "("
                    (stringify-ln* (add1 depth) (car l)) ")" 
                    (stringify-ln* depth (cdr l)))))))))

(define print-S
  (lambda (s)
    (display (string-append "(" s ")"))))
; (print-S (stringify* '(a (b) c))) ;=> (a (b) c)
; (print-S (stringify* '(a ((b)) c))) ;=> (a ((b)) c)

;; ex: of new line in string
; (display "hello\ntest\n")








;; ex: on how to get input!!
; (display "Please enter your name: ")
; (define name (read))
; (display "Hello, ")
; (display name)
; (newline)








;; room example
;; (optional #) (x y) 'symbol "item" 'action '() 
;;        '() <-- output/items when item has action done
;; 'symbol prints, else ( )
(define room-data
   '(   ((6 8) "@" "player")
        ((2 1) "c" "chair"  break ( (4 "(x)" "wooden pole") ;<- need symbol??
                                      (2 "wooden panel")))
        ((1 2) "c" "chair"  break ( (4 "wooden pole")
                                      (2 "wooden panel")))
        ((2 3) "c" "chair"  break ( (4 "wooden pole")
                                      (2 "wooden panel")))
        ((3 2) "c" "chair"  break ( (4 "wooden pole")
                                      (2 "wooden panel")))
        ;; second set of chairs
        ((7 1) "c" "chair"  break ( (4 "wooden pole")
                                      (2 "wooden panel")))
        ((6 2) "c" "chair"  break ( (4 "wooden pole")
                                      (2 "wooden panel")))
        ((7 3) "c" "chair"  break ( (4 "wooden pole")
                                      (2 "wooden panel")))
        ((8 2) "c" "chair"  break ( (4 "wooden pole")
                                      (2 "wooden panel")))
        ;; tables
        ((2 2) "T" "table" break ( (4 "wooden pole")
                                     (4 "wooden panel"))
                   on ( (1 "small vase" break (3 "ceramic shards"))
                        (1 "paper" read ("Dear tom..."))))
        ((7 2) "T" "table" break ( (4 "wooden pole")
                                     (4 "wooden panel"))
                   on ( (1 "small vase" break (3 "ceramic shards"))
                        (1 "paper" read ("Dear tom..."))))

        ((9 6) "C" "chest" break ( (1 "wooden pole")
                                     (6 "wooden panel"))
                   in ( (1 "paper" read ("Dear someone.."))
                        (37 "gold")))))
;; TODO: func that inputs room-data and outputs room
(define room-ex 
    '(  ((_)(1)(2)(3)(4)(5)(6)(7)(8)(9)(0))
        ((1)(_)(c)(_)(_)(_)(_)(_)(_)(_)(_))
        ((2)(c)(T)(c)(_)(_)(_)(_)(_)(_)(_))
        ((3)(_)(c)(_)(_)(_)(_)(x)(_)(_)(_))
        ((4)(_)(_)(_)(_)(_)(_)(_)(_)(_)(_))
        ((5)(_)(_)(_)(_)(_)(_)(_)(_)(_)(_))
        ((6)(_)(c)(_)(_)(_)(_)(_)(@)(_)(_))
        ((7)(c)(T)(c)(_)(_)(_)(_)(_)(_)(_))
        ((8)(_)(c)(_)(_)(_)(_)(_)(_)(_)(_))
        ((9)(_)(_)(_)(_)(_)(C)(_)(_)(_)(_))
        ((0)(_)(_)(_)(_)(_)(_)(_)(_)(_)(_))))
(define print-room
    (lambda (room)
        (display (stringify-ln* 0 room-ex))))
; (print-room room-ex)










;; TODO: add other craftable objs
(define crafable-objs 
   '(  ("chair" 
           ((4 "wooden plank")
            (2 "wooden panel")))
        ("table"
           ((4 "wooden plank")
            (4 "wooden panel")))
        ("small vase"
            (3 "ceramic shards"))  ))


(define backpack
    '( (1 "paper" read ("Dear adventurer!"))))



;; TODO: break
;;to help player remember how to fix thingsthey broke
;;        show craftable items (displays craftable-items)
;;        how (fix / craft / make) chair? 
;;        must have all items it breaks into
;; all items in/on a broken item will appear in main room 
;;             or outer in/on the same obj that it was broken on

;; TODO: move 
    ;; move takes (x, y) numbers..
    ;; changing position of player
;; TODO: take / put
    ;; takes item and (x, y) positon or player position as input and..
    ;; moves items from room to backpack / from backpack to room
   


;; TODO: read
     ;; only compatible w/ 1 
     ;; prints a message to console

;; TODO: show (craftable / "makeables" objects, backpack, room, info of room symbol)
;; takes string of what to display and prints to console
;; show (x, y)
;; ex: show (9, 6) => a chest is at (9, 6)
;; ex: show c => 8 chairs


;; TODO: ** parser / interpreter?? 
;; input strings and use to apply functions && print output
;; maybe have a list of possible commands? like show, move, take...
(define commands
    '(("move" left) (move right) (move up) (move down)
      (a d w s)
      (move x, y) (move x y)
      (take symbol) (take obj)
      (put symbol) (put obj)
      (show craftable objects) (show craftables)
      (show makeable objects) (show makeables)

      (show backpack) (show room) (show symbol) (show obj)
      (show room)

      (break symbol) (break obj)
      (craft symbol) (craft obj)
      (make symbol) (make obj)
      (fix symbol) (make obj)
      (read symbol) (read obj)))
;; ^^ these are the possible commands that could be 






(define parser
  (lambda ()
    (display "Enter command: ")
    (define input (read-line))
    (write input) (newline)

    (cond
      ((eqlist? (string->list "end\r") (string->list input)) "goodbye")
      ;; when testing always do string->to 
      (else (parser))
     )))

(parser)


; (eqlist? (string->list "end") (string->list "end"))
; (write (string->list "end")) ;=> (#\e #\n #\d)
; (write (string->list "end\r")) ;=> (#\e #\n #\d #\return)
;; TODO: write func to convert list of char to list of words
;; TODO: lchar->lword






;; ex: input
; (display "Please enter your name: ")
; (define name (read))
; (display "Hello, ")
; (display (string? name))
; name

; (define input (read-line (current-input-port) 'any))
; (string->symbol "hey testing this") ;=> '|hey testing this|

(define dialog 
  '( (what the they say) (options (a) (b   
        (what the person says (options () () ()))
  ) (c))) 
  )


