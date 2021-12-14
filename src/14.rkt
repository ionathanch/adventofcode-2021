#lang curly-fn racket

(require "../lib.rkt")

(define-values (template rules left-end right-end)
  (match-let ([(list `(,template*) rules) (problem-input-grouped-lines 14)])
    (define template (map char->symbol (string->list template*)))
    (values (for/hash ([left template]
                       [right (rest template)])
              (values (cons left right) 1))
            (for/hash ([rule rules])
              (match-let ([(list _ left right insert) (regexp-match #px"(\\w)(\\w) -> (\\w)" rule)])
                (values (cons (string->symbol left)
                              (string->symbol right))
                        (string->symbol insert))))
            (first template) (last template))))

(define (step template)
  (for/fold ([polymer (hash)])
            ([pair (hash-keys template)])
    (match-let* ([(cons left right) pair]
                 [insert (hash-ref rules pair)]
                 [count (hash-ref template pair)]
                 [polymer (hash-update polymer (cons left insert) #{+ % count} 0)]
                 [polymer (hash-update polymer (cons insert right) #{+ % count} 0)])
      polymer)))

(define (count n)
  (for/fold ([counts (hash right-end 1 left-end 1)]
             #:result (let ([counts (hash-values counts)])
                        (/ (- (maximum counts) (minimum counts)) 2)))
            ([(k v) ((iterate step n) template)])
    (match-let* ([(cons left right) k]
                 [counts (hash-update counts left  #{+ % v} 0)]
                 [counts (hash-update counts right #{+ % v} 0)])
      counts)))

(show-solution (count 10) (count 40))