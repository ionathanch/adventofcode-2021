#lang curly-fn racket

(require "../lib.rkt")

(define order
  (~> (problem-input 4 "-order")
      first
      (string-split ",")
      (map string->number _)))

(define boards
  (~> (problem-input-grouped 4)
      (map string-lines _)
      (map #{map string-words %} _)
      (map #{map #{map string->number %} %} _)))

(define (winner? board)
  (or (ormap #{andmap false? %} board)
      (ormap #{andmap (λ (row) (false? (list-ref row %))) board} (range 5))))

(define (blot board n)
  (for/list ([row board])
    (for/list ([m row])
      (if (equal? n m)
          #f m))))

(define (score board)
  (sum (filter number? (apply append board))))

(define-values (winner n)
  (for/fold ([boards boards]
             [winner #f]
             [winning #f]
             #:result (values winner winning))
            ([n order]
             #:break winner)
    (define boards*
      (for/list ([board boards])
        (blot board n)))
    (define winner
      (findf winner? boards*))
    (values boards* winner n)))

(define-values (loser m)
  (for/fold ([boards boards]
             [losing #f]
             #:result (values (first boards) losing))
            ([m order]
             #:break (and (singleton? boards)
                          (winner? (first boards))))
    (define boards*
      (for/list ([board (filter (∘ not winner?) boards)])
        (blot board m)))
    (values boards* m)))

(define part1
  (* n (score winner)))

(define part2
  (* m (score loser)))

(show-solution part1 part2)
