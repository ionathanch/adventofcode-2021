#lang racket

(require "../lib.rkt")

;; raw is one of:
;; - number?
;; - (list raw? raw?)

;; raw?
(define input
  (for/list ([line (problem-input 18)])
    (read (open-input-string (string-replace line "," " ")))))

;; type: (or/c 'root 'left 'right)
;; parent: (or/c pair? #f)
;; left right: (or/c number? pair?)
(struct pair (type parent left right) #:mutable #:transparent)

;; (or/c number? pair?) (or/c number? pair?) -> pair?
;; Combine two pairs into a root pair
(define (combine left right)
  (define root (pair 'root #f left right))
  (when (pair? left)
    (set-pair-parent! left root)
    (set-pair-type! left 'left))
  (when (pair? right)
    (set-pair-parent! right root)
    (set-pair-type! right 'right))
  root)

;; raw? -> pair?
(define (raw->pair x)
  (match x
    [(? number? n) n]
    [(list left right)
     (define left-pair  (raw->pair left))
     (define right-pair (raw->pair right))
     (combine left-pair right-pair)]))

;; pair? -> string?
(define (pair->string p)
  (if (number? p)
      (format "~a" p)
      (format "[~a,~a]"
              (pair->string (pair-left p))
              (pair->string (pair-right p)))))

;; EXPLODE ;;

;; number? pair? -> void?
;; Effect: Adds given number to deepest left node
(define (add-leftmost! n p)
  (define left (pair-left p))
  (if (number? left)
      (set-pair-left! p (+ left n))
      (add-leftmost! n left)))

;; number? pair? -> void?
;; Effect: Adds given number to deepest right node
(define (add-rightmost! n p)
  (define right (pair-right p))
  (if (number? right)
      (set-pair-right! p (+ right n))
      (add-rightmost! n right)))

;; number? pair? -> void?
;; Effect: Adds given number to deepest left node in right node
(define (right-add-leftmost! n p)
  (define right (pair-right p))
  (if (number? right)
      (set-pair-right! p (+ right n))
      (add-leftmost! n right)))

;; number? pair? -> void?
;; Effect: Adds given number to deepest right node in left node
(define (left-add-rightmost! n p)
  (define left (pair-left p))
  (if (number? left)
      (set-pair-left! p (+ left n))
      (add-rightmost! n left)))

;; number? pair? -> void?
;; Effect: Adds given number to deepest left node of closest right sibling,
;; or does nothing if there is no right sibling
(define (up-add-leftmost! n p)
  (match (pair-type p)
    ['root (void)]
    ['left (right-add-leftmost! n (pair-parent p))]
    ['right (up-add-leftmost! n (pair-parent p))]))

;; number? pair? -> void?
;; Effect: Adds given number to deepest right node of closest left sibling,
;; or does nothing if there is no left sibling
(define (up-add-rightmost! n p)
  (match (pair-type p)
    ['root (void)]
    ['right (left-add-rightmost! n (pair-parent p))]
    ['left (up-add-rightmost! n (pair-parent p))]))

;; number? pair? -> (or/c 'exploded 'continue)
;; Explodes pairs at depth 4 or greater
(define (explode depth p)
  (match p
    [(pair 'left parent (? number? left) (? number? right))
     #:when (>= depth 4)
     (right-add-leftmost! right parent)
     (up-add-rightmost! left parent)
     (set-pair-left! parent 0)
     'exploded]
    [(pair 'right parent (? number? left) (? number? right))
     #:when (>= depth 4)
     (left-add-rightmost! left parent)
     (up-add-leftmost! right parent)
     (set-pair-right! parent 0)
     'exploded]
    [(pair type parent (? number? left) (? number? right))
     'continue]
    [(pair type parent (? number? left) (? pair? right))
     (explode (add1 depth) right)]
    [(pair type parent (? pair? left) (? number? right?))
     (explode (add1 depth) left)]
    [(pair type parent (? pair? left) (? pair? right))
     (define results
       (list (explode (add1 depth) left)
             (explode (add1 depth) right)))
     (if (member 'exploded results) 'exploded 'continue)]))

;; SPLIT ;;

;; number? pair? -> void?
;; Effect: Sets left node to new pair consisting of halved number
;; rounded down and up
(define (split-left! left p)
  (define halved (/ left 2))
  (set-pair-left! p (pair 'left p (floor halved) (ceiling halved))))

;; number? pair? -> void?
;; Effect: Sets right node to new pair consisting of halved number
;; rounded down and up
(define (split-right! right p)
  (define halved (/ right 2))
  (set-pair-right! p (pair 'right p (floor halved) (ceiling halved))))

;; pair? -> (or/c 'split 'continue)
;; Splits leftmost number greater than 10 into a pair of halved numbers
(define (split p)
  (match p
    [(pair type parent (? number? left) (? number? right))
     (if (>= left 10)
         (begin (split-left! left p) 'split)
         (if (>= right 10)
             (begin (split-right! right p) 'split)
             'continue))]
    [(pair type parent (? number? left) (? pair? right))
     (if (>= left 10)
         (begin (split-left! left p) 'split)
         (split right))]
    [(pair type parent (? pair? left) (? number? right))
     (match (split left)
       ['split 'split]
       ['continue
        (if (>= right 10)
            (begin (split-right! right p) 'split)
            'continue)])]
    [(pair type parent (? pair? left) (? pair? right))
     (match (split left)
       ['split 'split]
       ['continue (split right)])]))

;; REDUCE ;;

;; pair? -> void?
;; Effect: Explodes then splits pair until a fixed point is reached
(define (reduce p)
  (let loop ([status 'exploded])
    (if (symbol=? status 'continue)
        (unless (symbol=? (split p) 'continue)
          (loop (explode 0 p)))
        (loop (explode 0 p)))))

;; MAGNITUDE ;;

;; (or/c number? pair?) -> number?
(define (magnitude p)
  (match p
    [(? number? n) n]
    [(pair _ _ left right)
     (+ (* 3 (magnitude left))
        (* 2 (magnitude right)))]))

;; SOLUTION ;;

(define part1
  (for/fold ([left (raw->pair (first input))]
             #:result (magnitude left))
            ([right (rest input)])
    (define p (combine left (raw->pair right)))
    (reduce p)
    p))

(define part2
  (for*/fold ([sum 0])
             ([left input]
              [right input])
    (define p (combine (raw->pair left) (raw->pair right)))
    (reduce p)
    (max sum (magnitude p))))

(show-solution part1 part2)