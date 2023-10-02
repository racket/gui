#lang info

(define collection 'multi)

(define deps '("srfi-lite-lib"
               "data-lib"
               ["icons" #:version "1.3"]
               ["base" #:version "8.3.0.3"]
               ["syntax-color-lib" #:version "1.5"]
               ["draw-lib" #:version "1.18"]
               ["snip-lib" #:version "1.5"]
               "wxme-lib"
               "pict-lib"
               "scheme-lib"
               ["scribble-lib" #:version "1.36"]
               ["string-constants-lib" #:version "1.41"]
               "option-contract-lib"
               "2d-lib"
               "compatibility-lib"
               "tex-table"
               "simple-tree-text-markup-lib"
               ("gui-i386-macosx" #:platform "i386-macosx")
               ("gui-x86_64-macosx" #:platform "x86_64-macosx" #:version "1.3")
               ("gui-ppc-macosx" #:platform "ppc-macosx")
               ("gui-aarch64-macosx" #:platform "aarch64-macosx")
               ("gui-win32-i386" #:platform "win32\\i386")
               ("gui-win32-x86_64" #:platform "win32\\x86_64")
               ("gui-win32-arm64" #:platform "win32\\arm64")
               ("gui-x86_64-linux-natipkg" #:platform "x86_64-linux-natipkg")))
(define build-deps '("at-exp-lib"
                     "rackunit-lib"))

(define pkg-desc "implementation (no documentation) part of \"gui\"")

(define pkg-authors '(mflatt robby))

(define version "1.71")

(define license
  '(Apache-2.0 OR MIT))
