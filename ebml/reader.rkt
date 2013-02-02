#lang racket

;; this module provides functions that parse ebml byte-strings

(provide (contract-out 
          [ebml-parse 
           (-> (or/c path-string? bytes?)
               (listof ebml-element?))]
          [ebml-parse/optimistic
           (-> (or/c path-string? bytes?)
               (listof ebml-element?))]))

(define fix? exact-nonnegative-integer?)
(define ebml-element?
  (flat-murec-contract 
   ([ebml-element? (list/c fix? (or/c bytes?
                                      (listof ebml-element?)))])
   ebml-element?))

;; a "front door" function that handles paths as
;; well as byte-strings
(define (ebml-parse in-thing)
  (cond [(bytes? in-thing) (parse-elements in-thing 0)]
        [(path-string? in-thing)
         (parse-elements (file->bytes in-thing) 0)]
        [else
         (raise-argument-error 'ebml-parse
                               "byte-string or file path"
                               0 in-thing)]))

;; a "front door" function that handles paths as
;; well as byte-strings
(define (ebml-parse/optimistic in-thing)
  (cond [(bytes? in-thing) (optimistic-parse in-thing)]
        [(path-string? in-thing)
         (optimistic-parse (file->bytes in-thing))]
        [else
         (raise-argument-error 'ebml-parse/optimistic
                               "byte-string or file path"
                               0 in-thing)]))



;; parse elements until running out of bytes; signal an error if
;; the byte-string doesn't end at the end of an element.
#;(: ebml-parse (Bytes Exact-Nonnegative-Integer -> (Listof Element)))
(define (parse-elements in-bytes posn)
  (cond [(= posn (bytes-length in-bytes)) empty]
        [else
         (match-define (list element new-posn)
           (parse-element in-bytes posn))
         (cons element (parse-elements in-bytes new-posn))]))

;; the optimistic parser just assumes that every element
;; contains a list of other elements, and falls back
;; to a byte string when parsing it signals an error.
;; this is an error-prone way to parse, because you may
;; have, e.g., a string that just happens to look like
;; a piece of encoded data. For better control, don't
;; use the optimistic-parser.

(define (optimistic-parse bytes-in)
  (with-handlers ([exn:fail? (lambda (exn) bytes-in)])
    (define sub-elements (ebml-parse bytes-in))
    (for/list ([elt sub-elements])
      (match-define (list header-id body-bytes) elt)
      (list header-id (optimistic-parse body-bytes)))))



;; NB: this implementation is suboptimal in that it copies the 
;; byte strings at each sub-element. In the worst case (deeply
;; nested structures), amortized analysis shows that a tree 
;; nested to depth 'n' with 'm' nodes total could wind up using 
;; n*m memory, which can't be worse than m log m, if I'm correct. 
;; if you wanted to do better, you could create a representation
;; for a subbytes, though the visitor would have to understand
;; and respect it.

;; actually, I'm a bit surprised that DrR doesn't provide an 
;; immutable-subbytes form that just creates a reference to
;; an existing immutable byte string....

;; of course, if you really wanted to do it up, you'd redo everything
;; using streams of bytes rather than a byte-string.

;; bytes? fix? -> (list/c ebml-element? fix?)
;; given a byte string and a location, parse a single ebml element and 
;; return the element and the new parse position
;(: parse-element : Bytes Exact-Nonnegative-Integer -> Parse-Result)
(define (parse-element in-bytes posn)
  (check-posn-exists in-bytes posn)
  (define first-byte (bytes-ref in-bytes posn))
  (match-define (list header-id-first-byte header-len) 
    (cond [(not (= 0 (bitwise-and #x80 first-byte)))
           (list (bitwise-and #x7F first-byte) 1)]
          [(not (= 0 (bitwise-and #x40 first-byte)))
           (list (bitwise-and #x3F first-byte) 2)]
          [(not (= 0 (bitwise-and #x20 first-byte)))
           (list (bitwise-and #x1F first-byte) 3)]
          [(not (= 0 (bitwise-and #x10 first-byte)))
           (list (bitwise-and #x0F first-byte) 4)]
          [else (error 'parse-element 
                       "not a legal header-id beginning byte: ~s"
                       first-byte)]))
  ;; covers first byte of len field as well... :
  (check-posn-exists in-bytes (+ header-len posn))
  (define other-bits (bytes->list
                      (subbytes in-bytes (add1 posn) (+ header-len posn))))
  (define header-id (+ (arithmetic-shift header-id-first-byte
                                         (* (length other-bits) 8))
                        (bytes->uint other-bits 0)))
  (cond [(= header-id (sub1 (expt 2 (* header-len 7))))
         (raise-argument-error 'parse-element 
                               (format "non-reserved header id at posn ~s" posn)
                               0 in-bytes posn)])
  (parse-data-len in-bytes (+ posn header-len) header-id))


;; bytes? fix? fix? -> (list/c ebml-element? fix?)
;; given a byte string, a location, and a header ID, parse the data length
;; and data and return a Parse-Result
#;(: parse-data-len : Bytes Exact-Nonnegative-Integer Exact-Nonnegative-Integer ->
   Parse-Result)
(define (parse-data-len in-bytes posn header-id)
  (define first-byte (bytes-ref in-bytes posn))
  (match-define (list data-len-first-byte data-len-len)
    (cond [(not (= 0 (bitwise-and #x80 first-byte)))
           (list (bitwise-and #x7F first-byte) 1)]
          [(not (= 0 (bitwise-and #x40 first-byte)))
           (list (bitwise-and #x3F first-byte) 2)]
          [(not (= 0 (bitwise-and #x20 first-byte)))
           (list (bitwise-and #x1F first-byte) 3)]
          [(not (= 0 (bitwise-and #x10 first-byte)))
           (list (bitwise-and #x0F first-byte) 4)]
          [(not (= 0 (bitwise-and #x08 first-byte)))
           (list (bitwise-and #x07 first-byte) 5)]
          [(not (= 0 (bitwise-and #x04 first-byte)))
           (list (bitwise-and #x03 first-byte) 6)]
          [(not (= 0 (bitwise-and #x02 first-byte)))
           (list (bitwise-and #x01 first-byte) 7)]
          [(not (= 0 (bitwise-and #x01 first-byte)))
           (list (bitwise-and #x00 first-byte) 8)]
          [else (error 'parse-data-len
                       "not a legal data-len beginning byte: ~s" first-byte)]))
  (check-posn-exists in-bytes (+ posn data-len-len -1))
  (define other-bits (bytes->list
                      (subbytes in-bytes (add1 posn) (+ data-len-len posn))))
  (define data-len (+ (arithmetic-shift data-len-first-byte
                                         (* (length other-bits) 8))
                       (bytes->uint other-bits 0)))
  (cond [(= data-len (sub1 (expt 2 (* data-len-len 7))))
         (error "reserved data length id: ~s for data length len: ~s" data-len data-len-len)])
  (check-posn-exists in-bytes (+ posn data-len-len data-len -1))
  (define data-bytes (subbytes in-bytes 
                               (+ posn data-len-len)
                               (+ posn data-len-len data-len)))
  (list (list header-id data-bytes) (+ posn data-len-len data-len)))



;; bytes? fix? -> void
;; check that the given position exists in the byte string, signal
;; an error otherwise.
(define (check-posn-exists in-bytes posn)
  (when (<= (bytes-length in-bytes) posn)
    (error 'check-posn-exists "parsing expected to find a byte at position ~s, but the byte string ended: ~e"
           posn
           in-bytes)))

(module+ test
  (require rackunit)
  (check-not-exn (lambda () (check-posn-exists #"abc" 2)))
  (check-exn (lambda (exn) #t)
             (lambda () (check-posn-exists #"abc" 3))))


;; parses a byte string as an unsigned integer. Works for lists
;; of funny lengths such as three or five.
;; (listof byte?) fix? -> fix?
;(: bytes->uint : (Listof Byte) Exact-Nonnegative-Integer -> Exact-Nonnegative-Integer)
(define (bytes->uint l accum)
  (cond [(empty? l) accum]
        [else (bytes->uint (rest l) 
                           (+ (first l) (arithmetic-shift accum 8)))]))

(module+ test
  (check-equal? (parse-element (bytes #b10110000 #b10000001 13) 0)
                (list (list #x30 (bytes 13)) 3)))



(module+ test
  (check-exn (lambda (exn) #t)
           (lambda () (parse-element (bytes #xFF) 0)))
(check-equal? (parse-element (bytes #b10000110 #b10000011 34 27 97) 0)
        (list (list 6 (bytes 34 27 97)) 5))
(check-equal? 
 (parse-element (bytes #b01000000 #b00000110 #b10000011 34 27 97) 0)
 (list (list 6 (bytes 34 27 97)) 6))
  
  (check-equal? 
   (ebml-parse (bytes #b01000000 #b00000110 #b10000011 34 27 97))
   (list (list 6 (bytes 34 27 97))))
  
  (check-equal? 
   (ebml-parse (bytes #b01000000 #b00000110 #b10000011 34 27 97
                      #b10000011 #b10001011 1 2 3 4 5 6 7 8 9 10 11))
   (list (list 6 (bytes 34 27 97))
         (list 3 (bytes 1 2 3 4 5 6 7 8 9 10 11))))
  
  (check-equal? 
   (ebml-parse (bytes #b01000000 #b00000110 #b10000011 34 27 97
                      #b10000011 #b10001011 
                      #b10000010 #b10000011 1 2 3
                      #b10000100 #b10000100 1 2 3 4))
   (list (list 6 (bytes 34 27 97))
         (list 3 (bytes #b10000010 #b10000011 1 2 3
                        #b10000100 #b10000100 1 2 3 4))))
  
  (check-equal? 
   (ebml-parse/optimistic
    (bytes #b01000000 #b00000110 #b10000011 34 27 97
           #b10000011 #b10001011 
           #b10000010 #b10000011 1 2 3
           #b10000100 #b10000100 1 2 3 4))
   (list (list 6 (bytes 34 27 97))
         (list 3 (list (list 2 (bytes 1 2 3))
                       (list 4 (bytes 1 2 3 4))))))
)



#|
;; Typed racket worked great... except I couldn't write
;; the type of s-expressions for the result type.
#;(require/typed rackunit
               [check-equal? (Any Any -> Any)]
               [check-exn ((Any -> Any) (-> Any) -> Any)])

#;(define-type Element (U Element/s
                        Element/r))

#;(struct: Element/r ([ID : Symbol]
                    [data : (U String
                               Integer
                               (Listof Element))])
  #:transparent)
#;(struct: Element/s ([ID : Exact-Nonnegative-Integer]
                    [data : (U Bytes (Listof Element))])
  #:transparent)

#;(struct: Parse-Result ([element : Element]
                       [bytes-pos : Exact-Nonnegative-Integer])
  #:transparent)

|#