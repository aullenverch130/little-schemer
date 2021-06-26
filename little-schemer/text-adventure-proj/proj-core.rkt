#lang racket

;; from book
(define atom? 
  (lambda (x) 
    (and (not (pair? x)) (not (null? x)))))
(define add1
  (lambda (x) 
    (+ x 1)))





; (define map-ex1 
;     '(  ("( )(1)(2)(3)(4)(5)(6)(7)(8)(9)(0)")
;         ("(1)( )(c)( )( )( )( )( )( )( )( )")
;         ("(2)(c)[t*(c)( )( )( )( )( )( )( )")
;         ("(3)( )(c)( )( )( )( )( )( )( )( )")
;         ("(4)( )( )( )( )( )( )( )( )( )( )")
;         ("(5)( )( )( )( )( )( )( )( )( )( )")
;         ("(6)( )(c)( )( )( )( )( )(@)( )( )")
;         ("(7)(c)[t](c)( )( )( )( )( )( )( )")
;         ("(8)( )(c)( )( )( )( )( )( )( )( )")
;         ("(9)( )( )( )( )( )( )( )( )( )( )")
;         ("(0)( )( )( )( )( )( )( )( )( )( )")))


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







;; room example
;; (optional #) (x y) 'symbol "item" 'action '() 
;;        '() <-- output/items when item has action done
;; 'symbol prints, else ( )
(define room
   '(   ((6 8) "(@)" "player")
        ((2 1) "(c)" "chair"  break ( (4 "(x)" "wooden pole") ;<- need symbol??
                                      (2 "wooden panel")))
        ((1 2) "(c)" "chair"  break ( (4 "wooden pole")
                                      (2 "wooden panel")))
        ((2 3) "(c)" "chair"  break ( (4 "wooden pole")
                                      (2 "wooden panel")))
        ((3 2) "(c)" "chair"  break ( (4 "wooden pole")
                                      (2 "wooden panel")))
        ;; second set of chairs
        ((7 1) "(c)" "chair"  break ( (4 "wooden pole")
                                      (2 "wooden panel")))
        ((6 2) "(c)" "chair"  break ( (4 "wooden pole")
                                      (2 "wooden panel")))
        ((7 3) "(c)" "chair"  break ( (4 "wooden pole")
                                      (2 "wooden panel")))
        ((8 2) "(c)" "chair"  break ( (4 "wooden pole")
                                      (2 "wooden panel")))
        ;; tables
        ((2 2) "[t*" "table" break ( (4 "wooden pole")
                                     (4 "wooden panel"))
                   on ( (1 "small vase" break (3 "ceramic shards"))
                        (1 "paper" read ("Dear tom..."))))
        ((7 2) "[t]" "table" break ( (4 "wooden pole")
                                     (4 "wooden panel"))
                   on ( (1 "small vase" break (3 "ceramic shards"))
                        (1 "paper" read ("Dear tom..."))))

        ((9 6) "[c*" "chest" break ( (1 "wooden pole")
                                     (6 "wooden panel"))
                   in ( (1 "paper" read ("Dear someone.."))
                        (37 "gold")))))
(define room-ex 
    '(  ("( )(1)(2)(3)(4)(5)(6)(7)(8)(9)(0)")
        ("(1)( )(c)( )( )( )( )( )( )( )( )")
        ("(2)(c)[t*(c)( )( )( )( )( )( )( )")
        ("(3)( )(c)( )( )( )( )( )( )( )( )")
        ("(4)( )( )( )( )( )( )( )( )( )( )")
        ("(5)( )( )( )( )( )( )( )( )( )( )")
        ("(6)( )(c)( )( )( )( )( )(@)( )( )")
        ("(7)(c)[t](c)( )( )( )( )( )( )( )")
        ("(8)( )(c)( )( )( )( )( )( )( )( )")
        ("(9)( )( )( )( )( )[c*( )( )( )( )")
        ("(0)( )( )( )( )( )( )( )( )( )( )")))
;; show (x, y)
;; ex: show (9, 6) => a chest is at (9, 6)
;; ex: show c => 8 chairs


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

;; TODO: show (craftable / makeable objects, backpack, room)
;; [ = there is something on/in this item
;; * = there is something readable on/in item
;; ex: show [t* = table 
;; takes string of what to display and prints to console



;; TODO: ** interpretor / parser?? do last!? 
;; input strings and use to apply functions && print output



(define maptest1
    '((()()(y))
      (()(x)())
      (()()())))

(define displaylist2
    (lambda (x)
        (cond  
            ((null? x) (newline))
            (else 
                (newline)
                (display (car x)) 
                (displaylist2 (cdr x))))))

(define print-room
    (lambda (lvl lat)
        (cond
            ((null? lat) (cons lvl '()))
            ((atom? (car lat)) (cons (car lat) (print-room lvl (cdr lat))))
            (else (cons (print-room (add1 lvl) (car lat)) 
                        (print-room lvl (cdr lat)))))))

;; TODO: how to swap out values? filter data?? 




