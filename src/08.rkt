#lang curly-fn racket

(require racket/set
         "../lib.rkt")

(define input
  (for/list ([entry (problem-input 8)])
    (match-let ([(list patterns outputs) (string-split entry " | ")])
      (list (map string->list (string-words patterns))
            (map (∘ #{sort % char<?} string->list) (string-words outputs))))))

(define part1
  (~>> input
       (map second)
       (map #{count #{(or/c 2 3 4 7) (length %)} %})
       sum))

(define (config patterns)
  (define (digit segments)
    (findf #{= (length %) segments} patterns))
  (define (digits segments)
    (filter #{= (length %) segments} patterns))
  (define-values (one four seven eight)
    (values (digit 2) (digit 4) (digit 3) (digit 7)))
  (define-values (fives sixes)
    (values (digits 5) (digits 6)))
  (define top
    (set-first (set-subtract seven one)))
  (define bottom
    (set-first (set-subtract (apply set-intersect fives) four `(,top))))
  (define top-left
    (set-first (set-subtract (apply set-intersect sixes) one `(,top ,bottom))))
  (define bottom-right
    (set-first (set-subtract (apply set-intersect sixes) `(,top ,bottom ,top-left))))
  (define top-right
    (set-first (set-remove one bottom-right)))
  (define middle
    (set-first (set-subtract four one `(,top-left))))
  (define bottom-left
    (set-first (set-subtract eight `(,top ,bottom ,top-left ,top-right ,bottom-right ,middle))))
  (hash (sort one   char<?) #\1
        (sort four  char<?) #\4
        (sort seven char<?) #\7
        (sort eight char<?) #\8
        (sort (set-subtract eight `(,top-left  ,bottom-right)) char<?) #\2
        (sort (set-subtract eight `(,top-left  ,bottom-left))  char<?) #\3
        (sort (set-subtract eight `(,top-right ,bottom-left))  char<?) #\5
        (sort (set-remove eight middle)      char<?) #\0
        (sort (set-remove eight top-right)   char<?) #\6
        (sort (set-remove eight bottom-left) char<?) #\9))

(define part2
  (for/sum ([entry input])
    (let ([mapping (config (first entry))])
      (~>> (second entry)
           (map #{hash-ref mapping %})
           list->string
           string->number))))

(show-solution part1 part2)