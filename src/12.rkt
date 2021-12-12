#lang curly-fn racket

(require racket/set
         "../lib.rkt")

(define input
  (for/fold ([caves (hash)])
            ([connection (problem-input 12)])
    (match-let* ([(list cave1 cave2) (string-split connection "-")]
                 [caves (hash-update caves cave1 #{set-add % cave2} (set))]
                 [caves (hash-update caves cave2 #{set-add % cave1} (set))])
      caves)))

(struct path+ (path small-caves twice?))

(define (small? cave)
  (char-lower-case? (string-ref cave 0)))

(define (get-paths once?)
  (let loop ([queue (list (path+ '("start") (set) once?))]
             [paths 0])
    (match queue
      ['() paths]
      [(list (path+ path small-caves twice?) queue ...)
       (define adjacent
         (hash-ref input (first path)))
       (define visitable
         (if twice?
             (set-subtract adjacent (set "start" "end") small-caves)
             (set-subtract adjacent (set "start" "end"))))
       (define queue*
         (for/list ([cave visitable])
           (path+ (cons cave path)
                  (if (small? cave) (set-add small-caves cave) small-caves)
                  (or (set-member? small-caves cave) twice?))))
       (define paths*
         (if (set-member? adjacent "end")
             (add1 paths) paths))
       (loop (append queue* queue) paths*)])))

(show-solution (get-paths #t) (get-paths #f))