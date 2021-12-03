#lang curly-fn racket

(require "../lib.rkt")

(define input (problem-input 3))

(define bits (string-length (first input)))

(define (nth-bit nums p n)
  (let ([bs (map #{string-ref % n} nums)])
    (if (p (count #{char=? % #\1} bs) (/ (length nums) 2))
        #\1 #\0)))

(define (squimsh p)
  (~> (range bits)
      (map #{nth-bit input p %} _)
      chars->binary))

(define (search p)
  (for/fold ([nums input]
             #:result (string->binary (first nums)))
            ([n (range bits)]
             #:break (singleton? nums))
    (define bit (nth-bit nums p n))
    (filter #{char=? bit (string-ref % n)} nums)))

(define part1
  (* (squimsh >=) (squimsh <)))

(define part2
  (* (search >=) (search <)))

(show-solution part1 part2)