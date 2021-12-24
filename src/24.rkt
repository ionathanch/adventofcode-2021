#lang racket

(require "../lib.rkt")

(define input
  (for/list ([line (problem-input 24)])
    (match (string-words line)
      [`(,instr ,var) (list (string->symbol instr) (string->symbol var))]
      [`(,instr ,var ,val)
       (list (string->symbol instr)
             (string->symbol var)
             (or (string->number val)
                 (string->symbol val)))])))

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

(define As '(14 10 13 -8 11 11 14 -11 14 -1 -8 -5 -16 -6))
(define Bs '(12 9 8 3 0 11 10 13 3 10 10 14 6 5))

(define (monad* model)
  (for/fold ([z 0])
            ([w model]
             [A As]
             [B Bs])
    (define x (if (= w (- (remainder z 26) A)) 0 1))
    (+ (* x (+ w B))
       (* (add1 (* x 25))
          (quotient z (if (positive? A) 1 26))))))

(define part1
  (let loop ([model 99999999999999])
    (define model* (number->digits model))
    (if (member 0 model*)
        (loop (sub1 model))
        (if (zero? (monad* model*))
            model
            (loop (sub1 model))))))

(show-solution #f #f)