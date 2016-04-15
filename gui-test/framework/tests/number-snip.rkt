#lang racket/base
(require "test-suite-utils.rkt")

(test
 'number-snip-convert-text
 (λ (x) (or (equal? "1/2" x) (equal? "0.5" x)))
 (lambda ()
   (queue-sexp-to-mred
    `((dynamic-require 'file/convertible 'convert)
      (number-snip:make-fraction-snip 1/2 #f)
      'text
      #f))))

(test
 'number-snip-convert-png
 bytes?
 (lambda ()
   (queue-sexp-to-mred
    `((dynamic-require 'file/convertible 'convert)
      (number-snip:make-fraction-snip 1/2 #f)
      'png-bytes
      #f))))
