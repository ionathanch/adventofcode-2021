#lang curly-fn racket

(require "../lib.rkt")

(define-values (template rules left-end right-end)
  (match-let* ([`((,template*) ,rules) (problem-input-grouped-lines 14)]
               [(and template `(,left-end ,@_ ,right-end)) (string->list template*)])
    (values (for/hash ([left template]
                       [right (rest template)])
              (values (cons left right) 1))
            (for/hash ([rule rules])
              (match-let ([`(,_ ,left ,right ,insert) (regexp-match #px"(\\w)(\\w) -> (\\w)" rule)])
                (values (cons (string-ref left 0)
                              (string-ref right 0))
                        (string-ref insert 0))))
            left-end right-end)))

(define (step template)
  (for/fold ([polymer (hash)])
            ([(k v) template])
    (define insert (hash-ref rules k))
    (~> polymer
        (hash-update (cons (car k) insert) #{+ % v} 0)
        (hash-update (cons insert (cdr k)) #{+ % v} 0))))

(define (count n)
  (for/fold ([counts (hash right-end 1 left-end 1)]
             #:result (#{/ (- (maximum %) (minimum %)) 2} (hash-values counts)))
            ([(k v) ((iterate step n) template)])
    (~> counts
        (hash-update (car k) #{+ % v} 0)
        (hash-update (cdr k) #{+ % v} 0))))

(show-solution (count 10) (count 40))