#lang racket

(require "../lib.rkt")

(define input
  (for/list ([line (problem-input 2)])
    (match (string-split line " ")
      [`(,dir ,val) (list (string->symbol dir) (string->number val))])))

(define part1
  (let ([horiz (sum (filter-map (match-lambda [`(forward ,n) n] [else #f]) input))]
        [ups   (sum (filter-map (match-lambda [`(up      ,n) n] [else #f]) input))]
        [downs (sum (filter-map (match-lambda [`(down    ,n) n] [else #f]) input))])
    (* horiz (- downs ups))))

(define part2
  (for/fold ([aim 0]
             [horiz 0]
             [depth 0]
             #:result (* horiz depth))
            ([dir-val input])
    (match dir-val
      [`(forward ,val) (values aim (+ horiz val) (+ depth (* aim val)))]
      [`(up   ,val) (values (- aim val) horiz depth)]
      [`(down ,val) (values (+ aim val) horiz depth)])))

(show-solution part1 part2)