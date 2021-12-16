#lang plai

(require "../lib.rkt")

(define-type Packet
  [literal (version number?) (value number?)]
  [operator (version number?) (ID number?) (subpackets (listof Packet?))])

(define (hex->bits hex)
  (match hex
    [#\0 "0000"] [#\1 "0001"] [#\2 "0010"] [#\3 "0011"]
    [#\4 "0100"] [#\5 "0101"] [#\6 "0110"] [#\7 "0111"]
    [#\8 "1000"] [#\9 "1001"] [#\A "1010"] [#\B "1011"]
    [#\C "1100"] [#\D "1101"] [#\E "1110"] [#\F "1111"]))

(define input
  (for/fold ([bits ""])
            ([hex (string->list (first (problem-input 16)))])
    (string-append bits (hex->bits hex))))

(define (parse-lit version raw)
  (let loop ([value ""]
             [raw raw])
    (if (char=? (string-ref raw 0) #\0)
        (let ([value (string-append value (substring raw 1 5))])
          (values (literal version (string->binary value))
                  (substring raw 5)))
        (loop (string-append value (substring raw 1 5))
              (substring raw 5)))))

(define (parse-op/length version ID raw)
  (define len (string->binary (substring raw 0 15)))
  (let loop ([packets '()]
             [subraw (substring raw 15 (+ len 15))])
    (if (string-empty? subraw)
        (values (operator version ID (reverse packets))
                (substring raw (+ len 15)))
        (let-values ([(packet subraw) (parse-packet subraw)])
          (loop (cons packet packets) subraw)))))

(define (parse-op/count version ID raw)
  (define n (string->binary (substring raw 0 11)))
  (for/fold ([packets '()]
             [raw (substring raw 11)]
             #:result (values (operator version ID (reverse packets)) raw))
            ([_ (range n)])
    (let-values ([(packet raw) (parse-packet raw)])
      (values (cons packet packets) raw))))

(define (parse-packet raw)
  (define version (string->binary (substring raw 0 3)))
  (match (string->binary (substring raw 3 6))
    [4 (parse-lit version (substring raw 6))]
    [ID
     (match (string-ref raw 6)
       [#\0 (parse-op/length version ID (substring raw 7))]
       [#\1 (parse-op/count version ID (substring raw 7))])]))

(define (version-sum packet)
  (type-case Packet packet
    [literal (version _) version]
    [operator (version _ packets) (+ version (sum (map version-sum packets)))]))

(define (interp-packet packet)
  (type-case Packet packet
    [literal (version value) value]
    [operator (version ID packets)
              (define values (map interp-packet packets))
              (match ID
                [0 (apply + values)]
                [1 (apply * values)]
                [2 (apply min values)]
                [3 (apply max values)]
                [5 (if (> (first values) (second values)) 1 0)]
                [6 (if (< (first values) (second values)) 1 0)]
                [7 (if (= (first values) (second values)) 1 0)])]))

(let-values ([(packet _) (parse-packet input)])
  (show-solution (version-sum packet) (interp-packet packet)))