#lang curly-fn racket

(require "../lib.rkt")

(define input
  (for/vector ([row (problem-input 9)])
    (for/vector ([col (string->list row)])
      (char->number col))))

(define rows (vector-length input))
(define cols (vector-length (vector-ref input 0)))

(define (grid-ref grid row col [default +inf.0])
  (if (or (< row 0) (>= row rows)
          (< col 0) (>= col cols))
      default
      (vector-ref (vector-ref grid row) col)))

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
  (for*/fold ([risk 0]
              [basins '()]
              #:result (values risk (apply * (take (sort basins >) 3))))
             ([row (range rows)]
              [col (range cols)])
    (if (low? row col)
        (values (+ (add1 (grid-ref input row col)) risk)
                (cons (basin row col) basins))
        (values risk basins))))

(show-solution part1 part2)