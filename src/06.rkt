#lang curly-fn racket

(require "../lib.rkt")

(define input
  (~> (problem-input 6)
      first
      string-csv
      (map string->number _)))

(define fish
  (for/list ([age (range 0 9)])
    (cons age (count #{= % age} input))))

(define (tick days)
  (for/fold ([fish fish]
             #:result (sum (map cdr fish)))
            ([_ (range days)])
    (for/list ([age (range 0 9)])
      (match age
        [6 (cons age (+ (assocf 0 fish)
                        (assocf 7 fish)))]
        [8 (cons age (assocf 0 fish))]
        [_ (cons age (assocf (add1 age) fish))]))))

(show-solution (tick 80) (tick 256))