#lang racket

(require "../lib.rkt")

(define input
  (for/list ([line (problem-input 2)])
    (match (string-split line " ")
      [`("up"   ,val) (list 'vert (negate (string->number val)))]
      [`("down" ,val) (list 'vert (string->number val))]
      [`("forward" ,val) (list 'horiz (string->number val))])))

(define part1
  (let ([horiz (sum (filter-map (match-lambda [`(horiz ,n) n] [else #f]) input))]
        [depth (sum (filter-map (match-lambda [`(vert  ,n) n] [else #f]) input))])
    (* horiz depth)))

(define part2
  (for/fold ([aim 0]
             [horiz 0]
             [depth 0]
             #:result (* horiz depth))
            ([dir-val input])
    (match dir-val
      [`(horiz ,val) (values aim (+ horiz val) (+ depth (* aim val)))]
      [`(vert  ,val) (values (+ aim val) horiz depth)])))

(show-solution part1 part2)