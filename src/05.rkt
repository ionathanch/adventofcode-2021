#lang curly-fn racket

(require "../lib.rkt")

(define input (problem-input 5))

(define lines
  (for/list ([line input])
    (~> line
        (regexp-match #px"(\\d+),(\\d+) -> (\\d+),(\\d+)" _)
        rest
        (map string->number _))))

(define-values (hvs diags)
  (partition #{or (= (first %) (third %))
                  (= (second %) (fourth %))} lines))

(define (count-crosses grid)
  (count #{> % 1} (hash-values grid)))

(define grid (make-hash))

(for ([hv hvs])
  (match hv
    [`(,x ,y1 ,x ,y2)
     (for ([y (range (min y1 y2) (add1 (max y1 y2)))])
       (hash-update! grid (list x y) add1 0))]
    [`(,x1 ,y ,x2 ,y)
     (for ([x (range (min x1 x2) (add1 (max x1 x2)))])
       (hash-update! grid (list x y) add1 0))]))

(define part1 (count-crosses grid))

(for ([diag diags])
  (match diag
    [`(,x1 ,y1 ,x2 ,y2)
     (define-values (x-offset x-step)
       (if (>= x2 x1) (values add1 1) (values sub1 -1)))
     (define-values (y-offset y-step)
       (if (>= y2 y1) (values add1 1) (values sub1 -1)))
     (for ([x (range x1 (x-offset x2) x-step)]
           [y (range y1 (y-offset y2) y-step)])
       (hash-update! grid (list x y) add1 0))]))

(define part2 (count-crosses grid))

(show-solution part1 part2)