#lang curly-fn racket

(require racket/set
         "../lib.rkt")

(define input
  (for/list ([entry (problem-input 8)])
    (match-let ([(list patterns outputs) (string-split entry " | ")])
      (list (map string->list (string-words patterns))
            (map string->list (string-words outputs))))))

(define part1
  (~>> input
       (map second)
       (map #{count #{(or/c 2 3 4 7) (length %)} %})
       sum))

(define ((config patterns) segs)
  (define (digit segments)
    (findf #{= (length %) segments} patterns))
  (define (digits segments)
    (filter #{= (length %) segments} patterns))
  (define-values (one four seven eight)
    (values (digit 2) (digit 4) (digit 3) (digit 7)))
  (define fives (apply set-intersect (digits 5)))
  (define sixes (apply set-intersect (digits 6)))
  (define three (set-union one  fives))
  (define nine  (set-union four sixes))
  (define five  (set-union (set-subtract four  one) sixes))
  (define six   (set-union (set-subtract eight one) sixes))
  (define zero  (set-union (set-subtract eight three) (set-subtract eight four) one))
  (define two   (set-union (set-subtract zero  sixes) fives))
  (cond
    [(set=? zero  segs) #\0]
    [(set=? one   segs) #\1]
    [(set=? two   segs) #\2]
    [(set=? three segs) #\3]
    [(set=? four  segs) #\4]
    [(set=? five  segs) #\5]
    [(set=? six   segs) #\6]
    [(set=? seven segs) #\7]
    [(set=? eight segs) #\8]
    [(set=? nine  segs) #\9]))

(define part2
  (for/sum ([entry input])
    (let ([mapping (config (first entry))])
      (~>> (second entry)
           (map mapping)
           list->string
           string->number))))

(show-solution part1 part2)