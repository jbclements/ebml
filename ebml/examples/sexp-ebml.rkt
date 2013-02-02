#lang racket

(require "../reader.rkt"
         "../writer.rkt"
         rackunit)

;; we'll encode pairs as ID 1, atoms as ID 2, and null as ID 3.

(define example-sexp '(a (b) c))
(define example-ebml
  '((1 ((2 #"a") (1 ((1 ((2 #"b") (3 #"")))
                     (1 ((2 #"c") (3 #"")))))))))

(check-equal?
 (ebml->bytes example-ebml)
 (bytes-append #"\201\223\202\201a\201\216\201"
               #"\205\202\201b\203\200\201\205"
               #"\202\201c\203\200"))

;; ping-pong required to know when to recur:
(define (read-ebml-sexps bytes)
  (map expand-ebml-sexp (ebml-read bytes)))

(define (expand-ebml-sexp element)
  (match element
    [(list 1 exps-bytes) 
     (apply cons (read-ebml-sexps exps-bytes))]
    [(list 2 atom-bytes)
     (string->symbol (bytes->string/utf-8 atom-bytes))]
    [(list 3 atom-bytes)
     empty]))

;; round-trip test:
(check-equal?
 (read-ebml-sexps (ebml->bytes example-ebml))
 (list '(a (b) c)))