#lang racket

(require "../lib.rkt")

(define x-min 88)
(define x-max 125)
(define y-min -157)
(define y-max -103)

(define part1
  (let ([δy (sub1 (abs y-min))])
    (/ (* δy (add1 δy)) 2)))

(define δy-max (sub1 (abs y-min)))
(define δy-min y-min)
(define δx-max x-max)
(define δx-min (floor (sqrt (* x-min 2))))

(define (reaches-target? δx δy)
  (let loop ([x   0] [y   0]
             [δx δx] [δy δy])
    (cond
      [(or  (> x x-max) (< y y-min)) #f]
      [(and (<= x-min x x-max) (<= y-min y y-max)) #t]
      [else (loop (+ x δx) (+ y δy)
                  (if (zero? δx) δx (sub1 δx))
                  (sub1 δy))])))

(define part2
  (for*/sum ([δx (range* δx-min δx-max)]
             [δy (range* δy-min δy-max)]
             #:when (reaches-target? δx δy)) 1))

(show-solution part1 part2)