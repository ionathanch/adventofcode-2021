#lang curly-fn racket

(require "../lib.rkt")

(define input
  (for/vector ([row (problem-input 11)])
    (for/vector ([col (string->list row)])
      (char->number col))))

(define rows (vector-length input))
(define cols (vector-length (vector-ref input 0)))
(define total (* rows cols))

(define (grid-ref grid row col)
  (vector-ref (vector-ref grid row) col))

(define (grid-update! grid row col f)
  (let ([vec (vector-ref grid row)])
    (vector-set! vec col (f (vector-ref vec col)))))

(define (grid-set! grid row col v)
  (grid-update! grid row col (const v)))

(define (adjs row col)
  (for*/list ([r (range* (max 0 (sub1 row)) (min (sub1 rows) (add1 row)))]
              [c (range* (max 0 (sub1 col)) (min (sub1 cols) (add1 col)))])
    (cons r c)))

(define (incr! octopodes)
  (for* ([row (range rows)]
         [col (range cols)])
    (grid-update! octopodes row col add1)))

(define (flash! flashed octopodes)
  (for*/fold ([newly-flashed (set)])
             ([row (range rows)]
              [col (range cols)])
    (if (and (not (set-member? flashed (cons row col)))
             (> (grid-ref octopodes row col) 9)
             (for ([adj (adjs row col)])
               (grid-update! octopodes (car adj) (cdr adj) add1)))
        (set-add newly-flashed (cons row col))
        newly-flashed)))

(define (decharge! octopodes)
  (for* ([row (range rows)]
         [col (range cols)])
    (when (> (grid-ref octopodes row col) 9)
      (grid-set! octopodes row col 0))))

(define (step! octopodes)
  (incr! octopodes)
  (define flashes
    (let loop ([flashed (set)])
      (let ([newly-flashed (flash! flashed octopodes)])
        (if (set-empty? newly-flashed)
            (set-count flashed)
            (loop (set-union newly-flashed flashed))))))
  (decharge! octopodes)
  flashes)

(define part1
  (for/sum ([_ (range 100)])
    (step! input)))

(define part2
  (for/last ([steps (in-naturals 101)]
             #:final (= (step! input) total))
    steps))

(show-solution part1 part2)