#lang racket

;; this module provides functions that parse ebml byte-strings

(provide (contract-out 
          [ebml-read 
           (-> (or/c path-string? bytes? port?)
               (listof ebml-element?))]
          [ebml-read/optimistic
           (-> (or/c path-string? bytes? port?)
               (listof ebml-element?))]))

(define fix? exact-nonnegative-integer?)
(define ebml-element?
  (flat-murec-contract 
   ([ebml-element? (list/c fix? (or/c bytes?
                                      (listof ebml-element?)))])
   ebml-element?))

;; a "front door" function that handles paths as
;; well as byte-strings
(define (ebml-read in-thing)
  (cond [(bytes? in-thing) (parse-elements 
                            (open-input-bytes in-thing))]
        [(path-string? in-thing)
         (define in-port (open-input-file in-thing))
         (begin0 (parse-elements in-port)
                 (close-input-port in-port))]
        [(port? in-thing)
         (parse-elements in-thing)]
        [else
         (raise-argument-error 'ebml-read
                               "byte-string or file path or port"
                               0 in-thing)]))

;; a "front door" function that handles paths as
;; well as byte-strings
(define (ebml-read/optimistic in-thing)
  (cond [(bytes? in-thing) (optimistic-parse 
                            (open-input-bytes in-thing)
                            #f)]
        [(path-string? in-thing)
         (define in-port (open-input-file in-thing))
         (begin0 (optimistic-parse in-port #f)
                 (close-input-port in-port))]
        [(port? in-thing)
         (optimistic-parse in-thing #f)]
        [else
         (raise-argument-error 'ebml-read/optimistic
                               "byte-string or file path"
                               0 in-thing)]))



;; parse elements until running out of bytes; signal an error if
;; the byte-string doesn't end at the end of an element.
#;(: ebml-read (Bytes Exact-Nonnegative-Integer -> (Listof Element)))
(define (parse-elements port)
  (cond [(eof-object? (peek-byte port)) empty]
        [else
         (cons (parse-element port)
               (parse-elements port))]))

;; the optimistic parser just assumes that every element
;; contains a list of other elements, and falls back
;; to a byte string when parsing it signals an error.
;; this is an error-prone way to parse, because you may
;; have, e.g., a string that just happens to look like
;; a piece of encoded data. For better control, don't
;; use the optimistic-parser.

(define (optimistic-parse port fallback-bytes)
  (with-handlers ([exn:fail? (lambda (exn) 
                               (or fallback-bytes
                                   (raise exn)))])
    (define sub-elements (ebml-read port))
    (for/list ([elt sub-elements])
      (match-define (list header-id body-bytes) elt)
      (list header-id (optimistic-parse 
                       (open-input-bytes body-bytes)
                       body-bytes)))))

;; bytes? fix? -> (list/c ebml-element? fix?)
;; given a byte string and a location, parse a single ebml element and 
;; return the element and the new parse position
;(: parse-element : Bytes Exact-Nonnegative-Integer -> Parse-Result)
(define (parse-element port)
  (define first-byte (read-byte port))
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
  (define header-followup-bytes 
    (read-bytes (sub1 header-len) port))
  (when (< (bytes-length header-followup-bytes) (sub1 header-len))
    (raise-argument-error 'parse-element 
                          "port containing complete header id"
                          0 port))
  (define other-bits (bytes->list header-followup-bytes))
  (define header-id (+ (arithmetic-shift header-id-first-byte
                                         (* (length other-bits) 8))
                        (bytes->uint other-bits 0)))
  (cond [(= header-id (sub1 (expt 2 (* header-len 7))))
         (raise-result-error 'parse-element
                             "non-reserved header id"
                             header-id)])
  (parse-data-len port header-id))


;; port? fix? -> ebml-element?
;; given a byte string, a location, and a header ID, parse the data length
;; and data and return a Parse-Result
(define (parse-data-len port header-id)
  (define first-byte (read-byte port))
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
  (define followup-data-len-bytes 
    (read-bytes (sub1 data-len-len) port))
  (when (< (bytes-length followup-data-len-bytes) 
           (sub1 data-len-len))
    (raise-argument-error
     'parse-data-len "port containing complete data length"
     0 port header-id))
  (define other-bits (bytes->list followup-data-len-bytes))
  (define data-len (+ (arithmetic-shift data-len-first-byte
                                         (* (length other-bits) 8))
                       (bytes->uint other-bits 0)))
  (cond [(= data-len (sub1 (expt 2 (* data-len-len 7))))
         (error "reserved data length id: ~s for data length len: ~s" data-len data-len-len)])
  (define data-bytes (read-bytes data-len port))
  (when (< (bytes-length data-bytes) 
           (sub1 data-len))
    (raise-argument-error
     'parse-data-len "port containing complete data"
     0 port header-id))
  (list header-id data-bytes))



;; parses a byte string as an unsigned integer. Works for lists
;; of funny lengths such as three or five.
;; (listof byte?) fix? -> fix?
;(: bytes->uint : (Listof Byte) Exact-Nonnegative-Integer -> Exact-Nonnegative-Integer)
(define (bytes->uint l accum)
  (cond [(empty? l) accum]
        [else (bytes->uint (rest l) 
                           (+ (first l) (arithmetic-shift accum 8)))]))

(module+ test
  (require rackunit)
  (check-equal? 
   (parse-element 
    (open-input-bytes (bytes #b10110000 #b10000001 13)))
   (list #x30 (bytes 13))))



(module+ test
  (check-exn 
   (lambda (exn) #t)
   (lambda () (parse-element (open-input-bytes (bytes #xFF)))))
  (check-equal?
   (parse-element (open-input-bytes
                   (bytes #b10000110 #b10000011 34 27 97)))
   (list 6 (bytes 34 27 97)))
  (check-equal? 
   (parse-element 
    (open-input-bytes
     (bytes #b01000000 #b00000110 #b10000011 34 27 97)))
   (list 6 (bytes 34 27 97)))
  
  (check-equal? 
   (parse-elements
    (open-input-bytes
     (bytes #b01000000 #b00000110 #b10000011 34 27 97)))
   (list (list 6 (bytes 34 27 97))))
  
  (check-equal? 
   (ebml-read 
    (open-input-bytes
     (bytes #b01000000 #b00000110 #b10000011 34 27 97)))
   (list (list 6 (bytes 34 27 97))))
  
  (check-equal? 
   (ebml-read (bytes #b01000000 #b00000110 #b10000011 34 27 97
                      #b10000011 #b10001011 1 2 3 4 5 6 7 8 9 10 11))
   (list (list 6 (bytes 34 27 97))
         (list 3 (bytes 1 2 3 4 5 6 7 8 9 10 11))))
  
  (check-equal? 
   (ebml-read (bytes #b01000000 #b00000110 #b10000011 34 27 97
                      #b10000011 #b10001011 
                      #b10000010 #b10000011 1 2 3
                      #b10000100 #b10000100 1 2 3 4))
   (list (list 6 (bytes 34 27 97))
         (list 3 (bytes #b10000010 #b10000011 1 2 3
                        #b10000100 #b10000100 1 2 3 4))))
  
  (check-equal? 
   (ebml-read/optimistic
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