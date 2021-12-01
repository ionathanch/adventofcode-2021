#lang racket

(require "../lib.rkt")

(define input (map string->number (problem-input 1)))

(define (deltas depths)
  (for/list ([depth-prev depths]
             [depth-next (rest depths)])
    (- depth-next depth-prev)))

(define window-sums
  (for/list ([depth-prev input]
             [depth-curr (rest input)]
             [depth-next (rest (rest input))])
    (+ depth-prev depth-curr depth-next)))

(define part1
  (count positive? (deltas input)))

(define part2
  (count positive? (deltas window-sums)))

(show-solution part1 part2)
