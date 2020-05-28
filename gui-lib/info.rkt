#lang info

(define collection 'multi)

(define deps '("srfi-lite-lib"
               "data-lib"
               ["icons" #:version "1.3"]
               ["base" #:version "7.0.0.19"]
               "syntax-color-lib"
               ["draw-lib" #:version "1.13"]
               ["snip-lib" #:version "1.2"]
               "wxme-lib"
               "pict-lib"
               "scheme-lib"
               "scribble-lib"
               ["string-constants-lib" #:version "1.33"]
               "option-contract-lib"
               "2d-lib"
               "compatibility-lib"
               "tex-table"
               ("gui-i386-macosx" #:platform "i386-macosx")
               ("gui-x86_64-macosx" #:platform "x86_64-macosx" #:version "1.3")
               ("gui-ppc-macosx" #:platform "ppc-macosx")
               ("gui-win32-i386" #:platform "win32\\i386")
               ("gui-win32-x86_64" #:platform "win32\\x86_64")
               ("gui-x86_64-linux-natipkg" #:platform "x86_64-linux-natipkg")))
(define build-deps '("at-exp-lib"
                     "rackunit-lib"))

(define pkg-desc "implementation (no documentation) part of \"gui\"")

(define pkg-authors '(mflatt robby))

(define version "1.47")
