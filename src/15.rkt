#lang curly-fn racket

(require graph
         (except-in "../lib.rkt" transpose))

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

(for/fold ([weights (hash '(0 . 0) 0)]
           #:result (printf "Part 1: ~a\n" (hash-ref weights `(,(sub1 dim) . ,(sub1 dim)))))
          ([diag (range* 0 (+ dim dim))])
  (for/fold ([weights weights])
            ([row (range* 0 diag  1)]
             [col (range* diag 0 -1)]
             #:when (and (< row dim) (< col dim)))
    (for/fold ([weights weights])
              ([adj (adjs dim row col)])
      (match-let* ([(cons row* col*) adj]
                   [weight (+ (grid-ref row* col*)
                              (hash-ref weights (cons row col)))])
        (hash-update weights adj #{if (< weight %) weight %} +inf.0)))))

(define (make-graph dim)
  (define graph (weighted-graph/directed '()))
  (for* ([row (range dim)]
         [col (range dim)]
         [adj (adjs dim row col)])
    (add-directed-edge! graph (cons row col) adj (grid-ref (car adj) (cdr adj))))
  (displayln "Graph created.")
  graph)

(let*-values ([(dim) (* dim 5)]
              [(weights paths) (dijkstra (make-graph dim) '(0 . 0))])
  (printf "Part 2: ~a\n" (hash-ref weights `(,(sub1 dim) . ,(sub1 dim)))))