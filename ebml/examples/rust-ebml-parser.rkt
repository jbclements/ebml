#lang racket

;; here's an example. At first I designed the package
;; with a complex visitor pattern, but finally discovered
;; that the visitor was just a call to the main entry point,
;; so it was totally unnecessary.

(require "../reader.rkt")

(define (rust-ebml-parse bytes)
  (map rust-ebml-expand (ebml-parse bytes)))

(define (rust-ebml-expand element)
  (match-define (list header-id data) element)
  (case header-id
    [(5) `(int ,(bytes->sint data))]
    [(11) `(string ,(bytes->string/utf-8 data))]
    [(15) `(enum ,(rust-ebml-parse data))]
    [(16) `(enum-vid ,(bytes->uint data))]
    [(17) `(enum-body ,(rust-ebml-parse data))]
    [(18) `(vec ,(rust-ebml-parse data))]
    [(19) `(vec-len ,(bytes->uint data))]
    ;; fall-through assumes it's compound:
    [else (printf "id: ~s\n" header-id)
          (with-handlers
              ((exn:fail? 
                (lambda (exn)
                  (error 'rust-specific-parse
                         "parse of element with header id ~s failed: ~s"
                         header-id
                         (exn-message exn)))))
            (list header-id (rust-ebml-parse data)))]))



;; this one only works on "standard" lengths:
#;(: bytes->sint (Bytes -> Integer))
(define (bytes->sint bytes)
  (integer-bytes->integer bytes #t #t))

(define (bytes->uint bytes)
  (integer-bytes->integer bytes #f #t))


(module+ test
  (require rackunit)
  (check-equal? (bytes->uint (bytes 2 1 3 4)) #x02010304))


(rust-ebml-parse 
 #"\321\20\0\0\312\213\203foo\222\20\0\0\6\223\204\0\0\0\0\205\210\0\0\0\0\0\0\0\3\217\20\0\0\233\220\204\0\0\0\1\221\20\0\0\220\222\20\0\0\6\223\204\0\0\0\0\205\210\0\0\0\0\0\0\0\1\217\20\0\0\v\220\204\0\0\0\0\221\20\0\0\0\217\20\0\0\v\220\204\0\0\0\1\221\20\0\0\0\217\20\0\0\v\220\204\0\0\0\2\221\20\0\0\0\222\20\0\0\6\223\204\0\0\0\0\222\20\0\0\6\223\204\0\0\0\0\222\20\0\0\6\223\204\0\0\0\0\217\20\0\0\v\220\204\0\0\0\0\221\20\0\0\0\205\210\0\0\0\0\0\0\0\2\217\20\0\0\v\220\204\0\0\0\0\221\20\0\0\0\217\20\0\0\v\220\204\0\0\0\2\221\20\0\0\0")


;; should produce:
#;'((81
     ((string "foo")
      (vec ((vec-len 0)))
      (int 3)
      (enum
       ((enum-vid 1)
        (enum-body
         ((vec ((vec-len 0)))
          (int 1)
          (enum ((enum-vid 0) (enum-body ())))
          (enum ((enum-vid 1) (enum-body ())))
          (enum ((enum-vid 2) (enum-body ())))
          (vec ((vec-len 0)))
          (vec ((vec-len 0)))
          (vec ((vec-len 0)))
          (enum ((enum-vid 0) (enum-body ())))
          (int 2)
          (enum ((enum-vid 0) (enum-body ())))))))
      (enum ((enum-vid 2) (enum-body ()))))))