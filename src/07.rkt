#lang curly-fn racket

(require "../lib.rkt")

(define input
  (~>> (problem-input 7)
       first
       string-csv
       (map string->number)))

(define (total-fuel target fuel)
  (let ([target (floor (target input))])
    (sum (map #{fuel (abs (- % target))} input))))

(show-solution (total-fuel median identity)
               (total-fuel mean #{/ (* % (add1 %)) 2}))