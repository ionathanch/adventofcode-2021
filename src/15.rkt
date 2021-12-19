#lang curly-fn racket

(require graph
         (except-in "../lib.rkt" transpose))

(struct posn (row col) #:transparent)

(define input
  (for/vector ([row (problem-input 15)])
    (for/vector ([col (string->list row)])
      (char->number col))))

(define dim (vector-length input))

(define (grid-ref row col)
  (define row* (remainder row dim))
  (define col* (remainder col dim))
  (define offset (+ (quotient row dim) (quotient col dim)))
  (define v (+ offset (vector-ref (vector-ref input row*) col*)))
  (if (> v 9) (add1 (% v 10)) v))

(define (adjs dim row col)
  (filter #{and (<= 0 (car %) (sub1 dim)) (<= 0 (cdr %) (sub1 dim))}
          `((,(add1 row) . ,col) (,(sub1 row) . ,col) (,row . ,(add1 col)) (,row . ,(sub1 col)))))

(define (make-graph dim)
  (define graph (weighted-graph/directed '()))
  (for* ([row (range dim)]
         [col (range dim)]
         [adj (adjs dim row col)])
    (add-directed-edge! graph (posn row col)
                        (posn (car adj) (cdr adj))
                        (grid-ref (car adj) (cdr adj))))
  graph)

(define (shortest-path dim)
  (let*-values ([(weights paths) (dijkstra (make-graph dim) (posn 0 0))])
    (hash-ref weights (posn (sub1 dim) (sub1 dim)))))

(show-solution (time (shortest-path dim))
               (time (shortest-path (* dim 5))))