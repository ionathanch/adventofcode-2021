#lang racket

(require "../lib.rkt")

#;
(define input
  (for/list ([line (problem-input 24)])
    (match (string-words line)
      [`(,instr ,var) (list (string->symbol instr) (string->symbol var))]
      [`(,instr ,var ,val)
       (list (string->symbol instr)
             (string->symbol var)
             (or (string->number val)
                 (string->symbol val)))])))

#;
(define (monad model)
  (define (get state val)
    (if (number? val) val (hash-ref state val)))
  (define (monad model state instrs)
    (if (empty? instrs)
        (hash-ref state 'z)
        (match (first instrs)
          [`(inp ,var)
           (monad (rest model)
                  (hash-set state var (first model))
                  (rest instrs))]
          [`(add ,var ,val)
           (monad model
                  (hash-set state var
                            (+ (hash-ref state var)
                               (get state val)))
                  (rest instrs))]
          [`(mul ,var ,val)
           (monad model
                  (hash-set state var
                            (* (hash-ref state var)
                               (get state val)))
                  (rest instrs))]
          [`(div ,var ,val)
           (monad model
                  (hash-set state var
                            (quotient (hash-ref state var)
                                      (get state val)))
                  (rest instrs))]
          [`(mod ,var ,val)
           (monad model
                  (hash-set state var
                            (remainder (hash-ref state var)
                                       (get state val)))
                  (rest instrs))]
          [`(eql ,var ,val)
           (monad model
                  (hash-set state var
                            (if (= (hash-ref state var)
                                   (get state val))
                                1 0))
                  (rest instrs))])))
  (monad model (hash 'w 0 'x 0 'y 0 'z 0) input))

#| w3 == w4
 | w7  - 1 == w8
 | w10 - 2 == w9
 | w11 - 3 == w6
 | w5  - 5 == w12
 | w2  - 7 == w13
 | w14 - 6 == w1
 |#

(define part1
  (let* ([w2 9] [w3 9] [w4 9] [w5 9] [w7 9] [w10 9] [w11 9] [w14 9]
         [w13 (- w2 7)] [w12 (- w5 5)] [w8 (- w7 1)] [w9 (- w10 2)]
         [w6 (- w11 3)] [w1 (- w14 6)])
    (string->number
     (format "~a~a~a~a~a~a~a~a~a~a~a~a~a~a"
             w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14))))

(define part2
  (let* ([w1 1] [w3 1] [w4 1] [w6 1] [w8 1] [w9 1] [w12 1] [w13 1]
         [w14 (+ w1 6)] [w11 (+ w6 3)] [w7 (+ w8 1)] [w10 (+ w9 2)]
         [w5 (+ w12 5)] [w2 (+ w13 7)])
    (string->number
     (format "~a~a~a~a~a~a~a~a~a~a~a~a~a~a"
             w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14))))

(show-solution part1 part2)