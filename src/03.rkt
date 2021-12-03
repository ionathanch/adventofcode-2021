#lang curly-fn racket

(require "../lib.rkt")

(define input
  (for/list ([b (problem-input 3)])
    (string->list b)))

(define bits (length (first input)))

(define (nth-bit nums p n)
  (let ([bs (map #{list-ref % n} nums)])
    (if (p (count #{char=? % #\1} bs) (/ (length nums) 2))
        #\1 #\0)))

(define (squimsh p)
  (~> (range bits)
      (map #{nth-bit input p %} _)
      chars->binary))

(define (search p)
  (for/fold ([nums input]
             #:result (chars->binary (first nums)))
            ([n (range bits)]
             #:break (singleton? nums))
    (define bit (nth-bit nums p n))
    (filter #{char=? bit (list-ref % n)} nums)))

(define part1
  (* (squimsh >=) (squimsh <)))

(define part2
  (* (search >=) (search <)))

(show-solution part1 part2)