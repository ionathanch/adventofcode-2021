#lang curly-fn racket

(require racket/set
         "../lib.rkt")

(define input
  (~>> (problem-input 9)
       (map string->list)
       (map #{map char->number %})))

(define rows (length input))
(define cols (length (first input)))

(define (grid-ref grid row col [default +inf.0])
  (if (or (< row 0) (>= row rows)
          (< col 0) (>= col cols))
      default
      (list-ref (list-ref grid row) col)))

(define (neighbours row col)
  (list (cons (sub1 row) col)
        (cons (add1 row) col)
        (cons row (sub1 col))
        (cons row (add1 col))))

(define (low? row col)
  (andmap #{< (grid-ref input row col)
              (grid-ref input (car %) (cdr %))}
          (neighbours row col)))

(define (basinic seen loc)
  (list->set
   (filter #{and (< (grid-ref input (car %) (cdr %)) 9)
                 (not (set-member? seen %))}
           (neighbours (car loc) (cdr loc)))))

(define (basin row col)
  (let loop ([unseen (set (cons row col))]
             [seen (set)])
    (if (set-empty? unseen)
        (set-count seen)
        (let ([loc (set-first unseen)])
          (loop (set-union (basinic seen loc) (set-rest unseen))
                (set-add seen loc))))))

(define-values (part1 part2)
  (for/fold ([risk 0]
             [basins '()]
             #:result (values risk (apply * (take (sort basins >) 3))))
            ([row (range rows)])
    (for/fold ([risk risk]
               [basins basins])
              ([col (range cols)])
      (if (low? row col)
          (values (+ (add1 (grid-ref input row col)) risk)
                  (cons (basin row col) basins))
          (values risk basins)))))

(show-solution part1 part2)