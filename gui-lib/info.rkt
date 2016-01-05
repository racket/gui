#lang info

(define collection 'multi)

(define deps '("srfi-lite-lib"
               "data-lib"
               ["base" #:version "6.2.900.17"]
               "syntax-color-lib"
               ["draw-lib" #:version "1.11"]
               ["snip-lib" #:version "1.2"]
               "wxme-lib"
               "pict-lib"
               "scheme-lib"
               "scribble-lib"
               "string-constants-lib"
               "option-contract-lib"
               "2d-lib"
               "compatibility-lib"
               "tex-table"
               ("gui-i386-macosx" #:platform "i386-macosx")
               ("gui-x86_64-macosx" #:platform "x86_64-macosx")
               ("gui-ppc-macosx" #:platform "ppc-macosx")
               ("gui-win32-i386" #:platform "win32\\i386")
               ("gui-win32-x86_64" #:platform "win32\\x86_64")
               ("gui-x86_64-linux-natipkg" #:platform "x86_64-linux-natipkg")))
(define build-deps '("at-exp-lib"
                     "rackunit-lib"))

(define pkg-desc "implementation (no documentation) part of \"gui\"")

(define pkg-authors '(mflatt robby))

(define version "1.22")
