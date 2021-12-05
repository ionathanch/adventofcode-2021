#lang curly-fn racket

(require "../lib.rkt")

(define input (problem-input-grouped 4))

(define order
  (string-csv (first input) string->number))

(define boards
  (for/list ([board (rest input)])
    (for/list ([row (string-lines board)])
      (for/list ([cell (string-words row)])
        (string->number cell)))))

(define (winner? board)
  (or (ormap #{andmap false? %} board)
      ;; IMO this is the more aesthetic but less efficient version
      #;(ormap #{andmap false? %} (transpose board))
      (ormap #{not (ormap (∂r list-ref %) board)} (range (length board)))))

(define (blot board n)
  (for/list ([row board])
    (for/list ([m row])
      (if (equal? n m)
          #f m))))

(define (score board)
  (sum (filter number? (apply append board))))

(define-values (part1 part2)
  (for/fold ([boards boards]
             [winner #f]
             [winning #f]
             [loser #f]
             [losing #f]
             #:result (values (* (score winner) winning)
                              (* (score loser) losing)))
            ([m order]
             #:break (empty? boards))
    (define boards*
      (map #{blot % m} boards))
    (define-values (winners losers)
      (partition winner? boards*))
    (values losers
            (or winner (first* winners)) (if winner winning m)
            (first* winners) m)))

(show-solution part1 part2)