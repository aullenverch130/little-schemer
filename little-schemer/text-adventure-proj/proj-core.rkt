#lang racket


(define map 
    '((o o o o o)
      (o o o o o)
      (o o o o o)
      (o o o o o)))


(define map-ex1 
    '(  ("( )(1)(2)(3)(4)(5)(6)(7)(8)(9)(0)")
        ("(1)( )(c)( )( )( )( )( )( )( )( )")
        ("(2)(c)[t*(c)( )( )( )( )( )( )( )")
        ("(3)( )(c)( )( )( )( )( )( )( )( )")
        ("(4)( )( )( )( )( )( )( )( )( )( )")
        ("(5)( )( )( )( )( )( )( )( )( )( )")
        ("(6)( )(c)( )( )( )( )( )(@)( )( )")
        ("(7)(c)[t](c)( )( )( )( )( )( )( )")
        ("(8)( )(c)( )( )( )( )( )( )( )( )")
        ("(9)( )( )( )( )( )( )( )( )( )( )")
        ("(0)( )( )( )( )( )( )( )( )( )( )")))


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







;; break / fix game ??
;; # 'symbol "item" 'action '() <-- item/s when item has action done
;; all items in/on an item will appear at same lvl of item they are in/on if that item breaks
;; symbols have ' or * to denote if something is in/on the item ' or if you can read it *
(define room
   '(   (4 "c" "chair"  break ( (4 "wooden pole")
                                (2 "wooden panel")))
        (1 "t'" "table" break ( (4 "wooden pole")
                                (4 "wooden panel"))
                   on ( (1 "small vase" break (3 "ceramic shards"))
                        (1 "paper" read ("Dear tom..."))))
        (1 "c'*" "chest" break ( (1 "wooden pole")
                                 (6 "wooden panel"))
                    in ( (1 "paper" read ("Dear someone.."))
                         (37 "gold")))))

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

(define player
    '(pos (1 1)))
;; players position is in upper left corner..

;; TODO: break / (fix / craft)
;;      to fix you must have all items it breaks into
;;      ?? how help people remember how to fix things
;;            use craftable-items
;;            how (fix / craft / make) chair? 
;; all items in/on an item will appear at same lvl of item they are in/on if that item breaks

;; TODO: move 
    ;; move takes (x, y) numbers..
    ;; changing position of player
;; TODO: take / put
    ;; move items from backpack to room, from room to backpack
;; TODO: on / off
    ;; ?? how to compensate for break function

;; TODO: read
;;      only compatible w/ 1 

;; TODO: show (craftable / makeable objects, backpack, room)
;; ' = there is something on/in this item
;; * = there is something readable on/in item
;; ex: show t'* = table 



;; TODO: ** interpretor / parser?? do last!? 
;; input strings and use to apply functions && print output