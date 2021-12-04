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

(define part1
  (for/fold ([boards boards]
             [winner #f]
             [winning #f]
             #:result (* (score winner) winning))
            ([n order]
             #:break winner)
    (define boards*
      (map #{blot % n} boards))
    (values boards* (findf winner? boards*) n)))

(define part2
  (for/fold ([boards boards]
             [loser #f]
             [losing #f]
             #:result (* (score loser) losing))
            ([m order]
             #:break (empty? boards))
    (define boards*
      (map #{blot % m} boards))
    (values (filter (∘ not winner?) boards*) (first boards*) m)))

(show-solution part1 part2)