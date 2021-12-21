#lang curly-fn racket

(require "../lib.rkt")

(define-values (p1 p2) (values 1 6))

(define (% n m)
  (if (zero? (modulo n m)) m (modulo n m)))

(define (inc roll)
  (+ (% (+ roll 0) 100)
     (% (+ roll 1) 100)
     (% (+ roll 2) 100)))

(define part1
  (let loop ([roll 1]
             [rolls 0]
             [next (cons p1 0)]
             [prev (cons p2 0)])
    (define pos*
      (% (+ (car next) (inc roll)) 10))
    (define score*
      (+ (cdr next) pos*))
    (if (>= score* 1000)
        (* (cdr prev) (+ rolls 3))
        (loop (% (+ roll 3) 100) (+ rolls 3)
              prev (cons pos* score*)))))

(define rolls
  (hash 3 1
        4 3
        5 6
        6 7
        7 6
        8 3
        9 1))

(define part2
  (let loop ([univs (hash (list p1 0 p2 0) 1)]
             [wins1 0]
             [wins2 0])
    (cond
      [(hash-empty? univs) (max wins1 wins2)]
      [else
       (define-values (univs* wins1*)
         (for*/fold ([univs (hash)]
                     [wins wins1])
                    ([(pp u) univs]
                     [(r m) rolls])
           (match-define (list p1 s1 p2 s2) pp)
           (define p1* (% (+ p1 r) 10))
           (define s1* (+ s1 p1*))
           (define u* (* u m))
           (if (>= s1* 21)
               (values univs (+ wins u*))
               (values (hash-update univs (list p1* s1* p2 s2) #{+ % u*} 0) wins))))
       (define-values (univs** wins2*)
         (for*/fold ([univs (hash)]
                     [wins wins2])
                    ([(pp u) univs*]
                     [(r m) rolls])
           (match-define (list p1 s1 p2 s2) pp)
           (define p2* (% (+ p2 r) 10))
           (define s2* (+ s2 p2*))
           (define u* (* u m))
           (if (>= s2* 21)
               (values univs (+ wins u*))
               (values (hash-update univs (list p1 s1 p2* s2*) #{+ % u*} 0) wins))))
       (loop univs** wins1* wins2*)])))

(show-solution part1 part2)