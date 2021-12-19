#lang curly-fn racket

(require "../lib.rkt")

(define input
  (for/list ([group (problem-input-grouped-lines 19)])
    (for/set ([line (rest group)])
      (match-let ([(list _ x y z) (regexp-match #px"(.+),(.+),(.+)" line)])
        (map string->number (list x y z))))))

(define (set-map s f)
  (for/set ([x s])
    (f x)))

(define (delta beacon1 beacon2)
  (match* (beacon1 beacon2)
    [((list x1 y1 z1) (list x2 y2 z2))
     (list (- x1 x2) (- y1 y2) (- z1 z2))]))

(define (shift beacon change)
  (match* (beacon change)
    [((list xb yb zb) (list xc yc zc))
     (list (+ xb xc) (+ yb yc) (+ zb zc))]))

(define (magnitude change)
  (match-let ([(list x y z) change])
    (+ (abs x) (abs y) (abs z))))

(define (deltas region)
  (for*/set ([beacon1 region]
             [beacon2 region])
    (abs (magnitude (delta beacon1 beacon2)))))

(define rotations
  (for*/fold ([rotations '()])
             ([dx '(1 -1)]
              [dy '(1 -1)]
              [dz '(1 -1)])
    (append
     (if (= (* dx dy dz) 1)
         (list (match-lambda [`(,x ,y ,z) `(,(* x dx) ,(* y dy) ,(* z dz))])
               (match-lambda [`(,x ,y ,z) `(,(* y dy) ,(* z dz) ,(* x dx))])
               (match-lambda [`(,x ,y ,z) `(,(* z dz) ,(* x dx) ,(* y dy))]))
         (list (match-lambda [`(,x ,y ,z) `(,(* x dx) ,(* z dz) ,(* y dy))])
               (match-lambda [`(,x ,y ,z) `(,(* y dy) ,(* x dx) ,(* z dz))])
               (match-lambda [`(,x ,y ,z) `(,(* z dz) ,(* y dy) ,(* x dx))])))
     rotations)))

(define rotated-regions
  (for/hash ([region input])
    (values region (map #{set-map region %} rotations))))

(define (combine region1 region2)
  (and (>= (set-count (set-intersect (deltas region1) (deltas region2))) 66)
       (for*/or ([beacon1 region1]
                 [beacon2 region2])
         (define change (delta beacon1 beacon2))
         (define shifted-region2
           (set-map region2 #{shift % (delta beacon1 beacon2)}))
         (and (>= (set-count (set-intersect region1 shifted-region2)) 12)
              (cons (set-union region1 shifted-region2)
                    change)))))

(define-values (part1 part2)
  (time
   (let loop ([full-map (first input)]
              [scanners '()]
              [regions (rest input)])
     (if (empty? regions)
         (values (set-count full-map)
                 (for*/fold ([distance 0])
                            ([scanner1 scanners]
                             [scanner2 scanners]
                             #:when (not (equal? scanner1 scanner2)))
                   (max distance (magnitude (delta scanner1 scanner2)))))
         (for*/fold ([full-map full-map]
                     [scanners scanners]
                     [regions regions]
                     #:result (loop full-map scanners regions))
                    ([region regions]
                     [rotation (hash-ref rotated-regions region)])
           (define combined/scanner
             (combine full-map rotation))
           (if combined/scanner
               (values (car combined/scanner)
                       (cons (cdr combined/scanner) scanners)
                       (remove region regions))
               (values full-map scanners regions)))))))

(show-solution part1 part2)