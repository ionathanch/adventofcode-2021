#lang curly-fn racket

(require "../lib.rkt")

(struct line (axis coord))

(define-values (dots folds)
  (match-let ([(list dots folds) (problem-input-grouped-lines 13)])
    (values
     (for/set ([dot dots])
       (map string->number (string-csv dot)))
     (for/list ([fold folds])
       (match-let ([`(,_ ,axis ,coord) (regexp-match #px"fold along ([xy])=(\\d+)" fold)])
         (line (string->symbol axis)
               (string->number coord)))))))

(define (fold dots axis coord)
  (match axis
    ['x (for/set ([xy dots])
          (match-let ([(list x y) xy])
            (if (> x coord)
                (list (- (* coord 2) x) y)
                xy)))]
    ['y (for/set ([xy dots])
          (match-let ([(list x y) xy])
            (if (> y coord)
                (list x (- (* coord 2) y))
                xy)))]))

(define part1
  (let ([line (first folds)])
    (set-count (fold dots (line-axis line) (line-coord line)))))

(define folded
  (for/fold ([dots dots])
            ([line folds])
    (fold dots (line-axis line) (line-coord line))))

(show-solution part1 "")

(let ([xs (set-map folded first)]
      [ys (set-map folded second)])
  (for ([y (range* (minimum ys) (maximum ys))])
    (for ([x (range* (minimum xs) (maximum xs))])
      (if (set-member? folded (list x y))
          (display #\█)
          (display #\ )))
    (newline)))