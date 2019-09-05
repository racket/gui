#lang racket/base

(provide (all-defined-out))

;; Versioning information and other strings used in the WXME format

(define MRED-READER-STR #"#reader(lib\"read.ss\"\"wxme\")")
(define MRED-START-STR #"WXME")
(define MRED-FORMAT-STR #"01")
(define MRED-VERSION-STR #"09")
(define MRED-VERSION-RX #rx"^0[1-9]$")
