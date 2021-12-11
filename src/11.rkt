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
  (vector-set! (vector-ref grid row) col (f (grid-ref grid row col))))

(define (adjs grid row col)
  (for*/list ([r (range* (max 0 (sub1 row)) (min (sub1 rows) (add1 row)))]
              [c (range* (max 0 (sub1 col)) (min (sub1 cols) (add1 col)))])
    (cons r c)))

(define (incr! octopodes)
  (for ([row (range rows)])
    (for ([col (range cols)])
      (grid-update! octopodes row col add1))))

(define (flash! flashed octopodes)
  (for*/fold ([newly-flashed '()])
             ([row (range rows)]
              [col (range cols)])
    (if (and (not (member (cons row col) flashed))
             (> (grid-ref octopodes row col) 9)
             (for ([adj (adjs octopodes row col)])
               (grid-update! octopodes (car adj) (cdr adj) add1)))
        (cons (cons row col) newly-flashed)
        newly-flashed)))

(define (decharge! octopodes)
  (for ([row (range rows)])
    (for ([col (range cols)])
      (when (> (grid-ref octopodes row col) 9)
        (grid-update! octopodes row col (const 0))))))

(define (step! octopodes)
  (incr! octopodes)
  (define flashes
    (let loop ([flashed '()])
      (let ([newly-flashed (flash! flashed octopodes)])
        (if (empty? newly-flashed)
            (length flashed)
            (loop (append newly-flashed flashed))))))
  (decharge! octopodes)
  flashes)

(define part1
  (for/sum ([_ (range 100)])
    (step! input)))

(define part2
  (let loop ([steps 101])
    (if (= (step! input) total)
        steps
        (loop (add1 steps)))))

(show-solution part1 part2)