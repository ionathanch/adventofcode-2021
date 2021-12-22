#lang curly-fn racket

(require "../lib.rkt")

(define input
  (for/list ([step (problem-input 22)])
    (match-let ([(list _ on/off xmin xmax ymin ymax zmin zmax)
                 (regexp-match #px"(on|off) x=(.+)\\.\\.(.+),y=(.+)\\.\\.(.+),z=(.+)\\.\\.(.+)" step)])
      (list (string->symbol on/off)
            (string->number xmin) (string->number xmax)
            (string->number ymin) (string->number ymax)
            (string->number zmin) (string->number zmax)))))

(define init
  (filter #{andmap #{<= -50 % 50} (rest %)} input))

(define (intersection above below)
  (match-define (list on/offa xmina xmaxa ymina ymaxa zmina zmaxa) above)
  (match-define (list on/offb xminb xmaxb yminb ymaxb zminb zmaxb) below)
  (define on/off
    (match* (on/offa on/offb)
      [('on  'on)  'off]
      [('off 'on)  'off]
      [('on  'off) 'on]
      [('off 'off) 'on]))
  (define xmin (max xmina xminb))
  (define xmax (min xmaxa xmaxb))
  (define ymin (max ymina yminb))
  (define ymax (min ymaxa ymaxb))
  (define zmin (max zmina zminb))
  (define zmax (min zmaxa zmaxb))
  (and (<= xmin xmax)
       (<= ymin ymax)
       (<= zmin zmax)
       (list on/off xmin xmax ymin ymax zmin zmax)))

(define (volume cuboid)
  (match-define (list on/off xmin xmax ymin ymax zmin zmax) cuboid)
  (define vol
    (* (add1 (- xmax xmin))
       (add1 (- ymax ymin))
       (add1 (- zmax zmin))))
  (match on/off
    ['on vol]
    ['off (- vol)]))

(define (cuboids steps)
  (for/fold ([cuboids '()]
             #:result (sum (map volume cuboids)))
            ([step steps])
    (define intersections
      (filter identity (map #{intersection step %} cuboids)))
    (define cuboids*
      (append intersections cuboids))
    (match (first step)
      ['on (cons step cuboids*)]
      ['off cuboids*])))

(show-solution (cuboids init) (cuboids input))