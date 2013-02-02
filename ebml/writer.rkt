#lang racket

;; functions for writing ebml elements. Might as well 
;; use streams, this time.

(provide (contract-out
          [ebml->bytes (-> (listof ebml-element?) bytes?)]
          [ebml-write (->* ((listof ebml-element?)) 
                           (port?)
                           void?)]))

(define fix? exact-nonnegative-integer?)
(define ebml-element?
  (flat-murec-contract 
   ([ebml-element? (list/c fix? (or/c bytes?
                                      (listof ebml-element?)))])
   ebml-element?))

;; given a list of ebml-elements, write them to a byte string
(define (ebml->bytes elements)
  (define collector (open-output-bytes))
  (ebml-write elements collector)
  (get-output-bytes collector))

;; given a list of ebml elements and an optional port, write
;; the encoded representation to the port
(define (ebml-write elements [port (current-output-port)])
  (for ([element elements])
    (ebml-element-write element port)))

;; write a single element to a port
(define (ebml-element-write element [port (current-output-port)])
  (match-define (list id data) element)
  (ebml-header-id-write id port)
  (ebml-data-length-write (ebml-data-length data) port)
  (cond [(bytes? data) (display data port)]
        [(list? data)
         (for ([element data])
           (ebml-element-write element port))]))

;; given a header id and a port, write the encoded 
;; representation of the id to the port.
(define (ebml-header-id-write id port)
  ;; we've already checked that it's in range, so it's 
  ;; safe to just re-use the data-length encoder.
  (ebml-data-length-write id port))

;; given length and a port, write the encoded
;; representation of the id to the port.
(define (ebml-data-length-write data-len port)
  (define bytes-required (data-length-encoded-length data-len))
  (define length-tagged
    (+ (arithmetic-shift #x1 (* bytes-required 7)) data-len))
  (display (uint->bytes length-tagged bytes-required) port))

;; fix? fix? -> (listof byte?)
(define (uint->bytes uint bytes)
  (list->bytes (reverse (u->b/helper uint bytes))))

(define (u->b/helper uint bytes)
  (cond [(= bytes 0) empty]
        [else (cons (bitwise-and #xFF uint)
                    (u->b/helper (arithmetic-shift uint -8)
                                 (sub1 bytes)))]))

;; how many bytes will it take to represent this element?
(define (ebml-element-length element)
  (define sub-length (ebml-data-length (second element)))
  (+ (header-encoded-length (first element))
     (data-length-encoded-length sub-length)
     sub-length))

(define (ebml-data-length data)
  (match data
    [(? bytes? b) (bytes-length b)]
    [(? list? elts)
     (for/sum ([elt elts]) (ebml-element-length elt))]))

(define (header-encoded-length header-id)
  (cond [(< header-id (sub1 (expt 2 7))) 1]
        [(< header-id (sub1 (expt 2 14))) 2]
        [(< header-id (sub1 (expt 2 21))) 3]
        [(< header-id (sub1 (expt 2 28))) 4]
        [else (raise-argument-error 
               'header-encoded-length
               "number smaller than 2^28 - 1"
               0 header-id)]))

(define (data-length-encoded-length data-length)
  (cond [(< data-length (sub1 (expt 2 7))) 1]
        [(< data-length (sub1 (expt 2 14))) 2]
        [(< data-length (sub1 (expt 2 21))) 3]
        [(< data-length (sub1 (expt 2 28))) 4]
        [(< data-length (sub1 (expt 2 35))) 5]
        [(< data-length (sub1 (expt 2 42))) 6]
        [(< data-length (sub1 (expt 2 48))) 7]
        [(< data-length (sub1 (expt 2 56))) 8]
        [else (raise-argument-error 
               'data-length-encoded-length
               "number smaller than 2^56 - 1"
               0 data-length)]))

(module+ test
  (require rackunit)
  (check-equal? (header-encoded-length 14) 1)
  (check-equal? (header-encoded-length 126) 1)
  (check-equal? (header-encoded-length 127) 2)
  (check-equal? (header-encoded-length (expt 2 13)) 2)
  (check-equal? (header-encoded-length (expt 2 14)) 3)
  (check-equal? (data-length-encoded-length (expt 2 14)) 3)
  (check-equal? (data-length-encoded-length (expt 2 41)) 6)
  (check-equal? (ebml-element-length 
                 (list 129 (make-bytes 342 2)))
                (+ 2 2 342))
  (check-equal? (uint->bytes (expt 2 16) 3) (bytes 1 0 0))
  (check-equal? (uint->bytes (expt 2 15) 3) (bytes 0 128 0))
  (check-equal? (ebml-element-length 
                 (list 129 (list (list 6 (bytes 1 2 3))
                                 (list 7 (bytes 1 2)))))
                (+ 2 1 1 1 3 1 1 2))
  
  (define (writer->bytes f)
    (define b (open-output-bytes))
    (f b)
    (get-output-bytes b))
  
  (check-equal? 
   (writer->bytes (lambda (p) 
                    (ebml-element-write 
                     (list 6 (bytes 34 27 97)) p)))
   (bytes #b10000110 #b10000011 34 27 97))
  
  (check-equal? 
   (ebml->bytes
    (list (list 6 (bytes 34 27 97))
          (list 3 (list (list 2 (bytes 1 2 3))
                        (list 4 (bytes 1 2 3 4))))))
   (bytes #b10000110 #b10000011 34 27 97
          #b10000011 #b10001011 
          #b10000010 #b10000011 1 2 3
          #b10000100 #b10000100 1 2 3 4)))