#lang racket

(require
  threading
  (only-in data/queue
           make-queue
           enqueue!)
  (only-in racket/set
           list->set
           set->list))

(provide (all-from-out threading)
         (all-defined-out))


;; Function helpers ;;

(define ∘ compose)
(define ∂ curry)
(define ∂r curryr)

;; uncurry : (a1 -> ... -> an -> b) -> ((listof a) -> b)
(define uncurry (curry apply))
(define $ uncurry)

;; iterate : (a -> a) -> number -> (a -> a)
(define ((iterate f n) a)
  (let loop ([a a] [n n])
    (if (zero? n) a (loop (f a) (sub1 n)))))


;; IO helpers ;;

;; problem-input : number? -> (listof string?)
;; Return contents of input file input/xx.txt as lines of strings.
(define (problem-input n [suffix ""])
  (let* ([filename (~a n #:min-width 2 #:align 'right #:left-pad-string "0")]
         [path     (build-path ".." "input" (format "~a~a.txt" filename suffix))])
    (file->lines path)))

;; problem-input-all : number? -> string?
;; Return contents of input file input/xx.txt as a single string.
(define (problem-input-all n [suffix ""])
  (let* ([filename (~a n #:min-width 2 #:align 'right #:left-pad-string "0")]
         [path     (build-path ".." "input" (format "~a~a.txt" filename suffix))])
    (file->string path)))

;; problem-input-grouped : number? -> (listof string?)
;; Return contents of input file input/xx.txt as a list of strings,
;; where each string is a group of lines separated by newlines.
(define (problem-input-grouped n [suffix ""])
  (string-split (problem-input-all n suffix) "\n\n"))

;; problem-input-grouped-lines : number? -> (listof (listof string?))
;; Return contents of input file input/xx.txt as a list of a list of strings,
;; where each list is the group of lines separated by newlines.
(define (problem-input-grouped-lines n [suffix ""])
  (map (λ (group) (string-split group "\n")) (problem-input-grouped n suffix)))

;; show-solution : a -> b -> void
;; Print part1 and part2 on separate lines.
(define (show-solution part1 part2)
  (printf "Part 1: ~a\nPart 2: ~a\n" part1 part2))


;; String helpers ;;

;; string-empty? : string? -> boolean?
(define (string-empty? s)
  (string=? s ""))

;; string->number* : (or/c string? #f) -> (or/c number? #f)
(define string->number*
  (and/c string? string->number))

;; string->symbol* : (or/c string? #f) -> (or/c symbol? #f)
(define string->symbol*
  (and/c string? string->symbol))

;; string->binary : string? -> number?
;; Given a string representation of a binary number,
;; convert it to the number it represents
(define (string->binary str)
  (string->number (string-append "#b" str)))

;; chars->binary : (listof char?) -> number?
;; Given a list of characters representing the bits of a binary number,
;; convert it into the number it represents
(define chars->binary (∘ string->binary list->string))

;; char->number : char? -> number?
;; Convert a digit character into its integral value
(define (char->number c)
  (- (char->integer c) (char->integer #\0)))

;; char->symbol : char? -> symbol?
;; Convert a character into the corresponding symbol
(define (char->symbol c)
  (string->symbol (list->string (list c))))

;; string->vector : string? -> (vectorof char?)
(define (string->vector str)
  (list->vector (string->list str)))

;; string-replaces : string? -> (listof (list? string? string?)) -> string
;; Perform string replacements in order,
;; so that later replacments may affect earlier ones
(define (string-replaces str replaces)
  (if (empty? replaces)
      str
      (string-replaces (string-replace str
                                       (caar replaces)
                                       (cadar replaces))
                       (rest replaces))))

;; string-words : string? -> (listof string?)
(define (string-words str)
  (string-split str #rx" +"))

;; string-lines : string? -> (listof string?)
(define (string-lines str)
  (string-split str "\n"))

;; string-csv : string? -> [string? -> a] -> (listof a)
(define (string-csv str [f identity])
  (map f (string-split str ",")))


;; Char helpers ;;

;; nchar=? : char -> char -> boolean
(define (nchar=? c1 c2)
  (not (char=? c1 c2)))

;; char-alphanumeric? : char -> boolean
(define char-alphanumeric?
  (or/c char-alphabetic? char-numeric?))


;; Number helpers ;;

;; sum : (listof number) -> number
(define (sum ns) (apply + ns))

;; maximum : (listof number) -> number
(define (maximum ns) (apply max ns))

;; minimum : (listof number) -> number
(define (minimum ns) (apply min ns))

;; mean : (listof number) -> number
(define (mean ns)
  (/ (sum ns) (length ns)))

;; median : (listof number) -> number
(define (median ns)
  (list-ref (sort ns <) (floor (/ (length ns) 2))))

;; ≠ : number -> number -> boolean
(define (≠ n1 n2)
  (not (= n1 n2)))

;; nzero? : number -> boolean
(define (nzero? n)
  (not (zero? n)))

;; negate : number -> number
(define (negate n)
  (- 0 n))

;; pos-or-zero : number -> number
(define (pos-or-zero n)
  (if (negative? n) 0 n))

;; % : number -> number -> number
(define % modulo)

;; number->digits-reverse : number -> (listof number)
;; Return the digits of the given number in reverse order (i.e. RTL)
(define (number->digits-reverse n)
  (if (< n 10)
      (list n)
      (cons (remainder n 10)
            (number->digits-reverse (quotient n 10)))))

;; number->digits : number -> (listof number)
;; Return the digits of the given number (LTR)
(define (number->digits n)
  (reverse (number->digits-reverse n)))

;; digits->number : (listof number) -> number
;; Return the given digits as a number
(define (digits->number ns)
  (let loop ([n 0] [ns ns])
    (if (empty? ns) n
        (loop (+ (* n 10) (car ns)) (cdr ns)))))


;; List helpers ;;

;; singleton? : (listof any) -> boolean
(define (singleton? lst)
  (match lst
    [(list _) true]
    [else #f]))

;; snoc : (listof any) -> any -> (listof any)
;; Append element to the back of the list
(define (snoc lst v)
  (append lst (list v)))

;; range* : number? -> number? -> [number?] -> (streamof number?)
(define range* in-inclusive-range)

;; scanl : (a -> a -> a) -> (listof a) -> (listof a)
;; foldl that accumulates partial results in a list
(define (scanl f init lst)
  (reverse
   (foldl (λ (v lst)
            (cons (f v (first lst)) lst))
          (list init) lst)))

;; scanr : (a -> a -> a) -> (listof a) -> (listof a)
;; foldr that accumulates partial results in a list
(define (scanr f init lst)
  (reverse
   (foldr (λ (v lst)
            (cons (f v (first lst)) lst))
          (list init) lst)))

;; list-ref* : (listof a) -> number -> a -> (or/c a #f)
;; Same as list-ref, except a default value is provided
;; if the index is beyond the length of the list.
(define (list-ref* lst pos [failure-result #f])
  (if (>= pos (length lst))
      failure-result
      (list-ref lst pos)))

;; first* : (listof a) -> (or/c a #f)
;; Get first of list or default if empty
(define (first* lst [failure-result #f])
  (match lst
    ['() failure-result]
    [`(,hd ,@_) hd]))

;; assocf : a -> (listof (cons a b)) -> (or/c b #f)
;; Returns cdr if assoc succeeds, otherwise #f
(define (assocf a lst)
  (match (assoc a lst)
    [`(,_ . ,b) b]
    [else #f]))

;; repeat : number -> (listof any) -> (listof any)
(define (repeat m lst)
  (if (zero? m) '()
      (append lst (repeat (sub1 m) lst))))

;; chunks-of : (listof any) -> nonzero? -> (listof (listof any))
;; Partitions a list into lists of the given size in order,
;; with the final list possibly being smaller
;; e.g. '(1 2 3 4 5) 2 => '((1 2) (3 4) (5))
(define (chunks-of lst size)
  (if (< (length lst) size) lst
      (cons (take lst size)
            (chunks-of (drop lst size) size))))

;; transpose : (listof (listof any)) -> (listof (listof any))
;; Turns a list of lists into a list of lists of
;; the first elements of the lists, ..., the nth elements
;; where n is the length of the shortest list.
;; In short, it transposes a list of rows into a list of columns.
;; e.g. '((1 2 3 4)          '((1 5 8)
;;        (5 6 7)         =>   (2 6 9)
;;        (8 9 10 11 12))      (3 7 10))
(define (transpose lists)
  (let* ([min-len (apply min (map length lists))]
         [lists (map (λ (lst) (take lst min-len)) lists)])
    (apply map list lists)))

;; list->queue : (listof a) -> (queueof a)
;; Creates a queue and adds elements of list in order
(define (list->queue lst)
  (let ([Q (make-queue)])
    (for-each (∂ enqueue! Q) lst)
    Q))

;; unique : (listof a) -> (listof a)
;; Return a list of unique items; not guaranteed to be stable
(define (unique lst)
  (set->list (list->set lst)))


;; Vector helpers ;;

;; vector-first : (vectorof any) -> any
(define (vector-first vec)
  (vector-ref vec 0))

;; vector-last : (vectorof any) -> any
(define (vector-last vec)
  (vector-ref vec (sub1 (vector-length vec))))

;; vector-ref* : (vectorof any) -> number -> any -> any
;; Same as list-ref, except a default value is provided
;; if the index is beyond the length of the list.
(define (vector-ref* vec pos failure-result)
  (if (>= pos (vector-length vec))
      failure-result
      (vector-ref vec pos)))

;; vector-set!* : (vectorof any) -> number -> any -> (vectorof any)
;; Set the value at given index in a new vector, then return that vector
;; If the index is beyond the indices of the vector,
;; a vector that can accomodate that index is returned,
;; with all the original elements and the element at the index set
(define (vector-set!* vec pos v)
  (let ([new-vec (make-vector (max (vector-length vec) (add1 pos)))])
    (vector-copy! new-vec 0 vec)
    (vector-set! new-vec pos v)
    new-vec))

;; hash->vector : (hashof (number => a)) -> (vectorof a)
;; Convert an intmap into a mutable vector
(define (hash->vector hash [default 0])
  (let ([length (add1 (apply max (hash-keys hash)))])
    (build-vector length (λ (i) (hash-ref hash i default)))))

;; vector->hash : (vectorof a) -> (hashof (number => a))
;; Convert a vector into an immutable intmap
(define (vector->hash vec)
  (let ([kvs (map cons (range (vector-length vec)) (vector->list vec))])
    (make-immutable-hash kvs)))

;; vector-index-where : (vectorof a) -> (a -> boolean) -> a
;; Return the first element of the vector that satisfies the given predicate
(define (vector-index-where vec p)
  (for/first ([i (vector-length vec)]
              #:when (p (vector-ref vec i)))
    i))
