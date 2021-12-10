#lang curly-fn racket

(require "../lib.rkt")

(define input (map string->list (problem-input 10)))

(define (corrupted-score char)
  (assocf char '((#\) . 3) (#\] . 57) (#\} . 1197) (#\> . 25137))))
(define (incomplete-score char)
  (assocf char '((#\( . 1) (#\[ . 2) (#\{ . 3) (#\< . 4))))

(define (autocomplete-score stack)
  (for/fold ([score 0])
            ([char stack])
    (+ (* score 5) (incomplete-score char))))

(define-values (part1 part2)
  (for/fold ([corrupted 0]
             [incomplete '()]
             #:result (values corrupted (median incomplete)))
            ([line input])
    (let loop ([stack '()]
               [line line])
      (match line
        ['() (values corrupted (cons (autocomplete-score stack) incomplete))]
        [(cons char line)
         (match* (char stack)
           [((or #\( #\[ #\{ #\<) _) (loop (cons char stack) line)]
           [(#\) (cons #\( stack)) (loop stack line)]
           [(#\] (cons #\[ stack)) (loop stack line)]
           [(#\} (cons #\{ stack)) (loop stack line)]
           [(#\> (cons #\< stack)) (loop stack line)]
           [(_ _) (values (+ corrupted (corrupted-score char)) incomplete)])]))))

(show-solution part1 part2)