#lang racket

(require "../lib.rkt")

(define input (map string->list (problem-input 25)))

(define rows (length input))
(define cols (length (first input)))

(define grid
  (for/fold ([grid (hash)])
            ([i (range rows)]
             [row input])
    (for/fold ([grid grid])
              ([j (range cols)]
               [col row])
      (hash-set grid (cons i j) col))))

(define (grid-ref grid row col)
  (hash-ref grid (cons row col)))

(define (grid-set grid row col v)
  (hash-set grid (cons row col) v))

(define (move-east grid-old grid-new row col)
  (match (grid-ref grid-old row (% (add1 col) cols))
    [#\. (let* ([grid-new (grid-set grid-new row col #\.)]
                [grid-new (grid-set grid-new row (% (add1 col) cols) #\>)])
           grid-new)]
    [else grid-new]))

(define (move-south grid-old grid-new row col)
  (match (grid-ref grid-old (% (add1 row) rows) col)
    [#\. (let* ([grid-new (grid-set grid-new row col #\.)]
                [grid-new (grid-set grid-new (% (add1 row) rows) col #\v)])
           grid-new)]
    [else grid-new]))

(define part1
  (time
   (let loop ([grid grid]
              [steps 1])
     (define east-grid
       (for/fold ([grid* grid])
                 ([(rc loc) grid]
                  #:when (char=? loc #\>))
         (move-east grid grid* (car rc) (cdr rc))))
     (define south-grid
       (for/fold ([grid* east-grid])
                 ([(rc loc) east-grid]
                  #:when (char=? loc #\v))
         (move-south east-grid grid* (car rc) (cdr rc))))
     (if (equal? grid south-grid)
         steps
         (loop south-grid (add1 steps))))))

(show-solution part1 #f)