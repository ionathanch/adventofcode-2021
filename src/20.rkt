#lang curly-fn racket

(require "../lib.rkt")

(match-define (list input1 input2)
  (problem-input-grouped 20))

(define algorithm
  (list->vector (string->list input1)))

(define image
  (for/fold ([img (hash)])
            ([line (string-lines input2)]
             [row (range (length (string-lines input2)))])
    (for/fold ([img img])
              ([char (string->list line)]
               [col (range (string-length line))])
      (hash-set img (cons row col) char))))

(define (square row col)
  (for*/list ([r (range* (sub1 row) (add1 row))]
              [c (range* (sub1 col) (add1 col))])
    (cons r c)))

(define (image-bounds img)
  (define keys (hash-keys img))
  (define rows (map car keys))
  (define cols (map cdr keys))
  (values (minimum rows) (maximum rows)
          (minimum cols) (maximum cols)))

(define (compute pixels)
  (~>> (map #{match % [#\. "0"] [#\# "1"]} pixels)
       (apply string-append)
       string->binary
       (vector-ref algorithm)))

(define (show img)
  (define-values (min-row max-row min-col max-col)
    (image-bounds img))
  (for ([row (range* min-row max-row)])
    (for ([col (range* min-col max-col)])
      (display (hash-ref img (cons row col) #\.)))
    (newline)))

(define (enhance default img)
  (define-values (min-row max-row min-col max-col)
    (image-bounds img))
  (for*/fold ([img* (hash)])
             ([row (range* (sub1 min-row) (add1 max-row))]
              [col (range* (sub1 min-col) (add1 max-col))])
    (~>> (square row col)
         (map #{hash-ref img % default})
         compute
         (hash-set img* (cons row col)))))

(define (enhance* n)
  (time
   (for/fold ([img image]
              [default #\.]
              #:result (count #{char=? % #\#} (hash-values img)))
             ([_ (range n)])
     (values (enhance default img)
             (match default [#\. #\#] [#\# #\.])))))

(show-solution (enhance* 2) (enhance* 50))