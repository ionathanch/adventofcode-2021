#lang curly-fn racket

(require "../lib.rkt")

(define input
  (for/list ([b (problem-input 3)])
    (string->list b)))

(define chars->binary
  (∘ string->binary list->string))

(define (nth-bit nums p n)
  (let ([bs (map #{list-ref % n} nums)])
    (if (p (count (∂ char=? #\1) bs) (/ (length nums) 2))
        #\1 #\0)))

(define (squimsh p)
  (~> (range (length (first input)))
      (map #{nth-bit input p %} _)
      chars->binary))

(define (search p)
  (let loop ([nums input]
             [n 0])
    (if (= (length nums) 1)
        (chars->binary (first nums))
        (let ([bit (nth-bit nums p n)])
          (loop (filter #{char=? bit (list-ref % n)} nums) (add1 n))))))

(define part1
  (* (squimsh >=) (squimsh <)))

(define part2
  (* (search >=) (search <)))

(show-solution part1 part2)