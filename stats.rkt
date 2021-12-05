#lang curly-fn racket

(require threading)

(define rkt-files
  (filter #{regexp-match #rx".*\\.rkt$" (path->string %)}
          (directory-list "src/" #:build? #{build-path "src/" %})))

(define srcs
  (map (λ~>> file->lines
             (filter non-empty-string?))
       rkt-files))

(define src-lengths (map length srcs))

(define src-widths
  (~>> (apply append srcs)
       (map string-length)
       (sort _ <=)))

(define src-widths-string
  (string-join (map number->string src-widths) ","))