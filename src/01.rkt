#lang racket

(require "../lib.rkt")

(define input (map string->number (problem-input 1)))

(define (deltas nth-rest)
  (for/list ([depth-prev input]
             [depth-next (nth-rest input)])
    (- depth-next depth-prev)))

(define part1
  (count positive? (deltas cdr)))

(define part2
  (count positive? (deltas cdddr)))

(show-solution part1 part2)