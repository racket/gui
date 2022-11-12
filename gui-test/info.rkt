#lang info

(define collection 'multi)

(define deps '("base" "string-constants-lib"))
(define build-deps '("racket-index"
                     "scheme-lib"
                     "draw-lib"
                     "racket-test"
                     "sgl"
                     "snip-lib"
                     "wxme-lib"
                     "gui-lib"
                     "syntax-color-lib"
                     "rackunit-lib"
                     "pconvert-lib"
                     "compatibility-lib"
                     "sandbox-lib"
                     "simple-tree-text-markup-lib"
                     "pict-lib"
                     "pict-snip-lib"))
(define update-implies '("gui-lib"))

(define pkg-desc "tests for \"gui\"")

(define pkg-authors '(mflatt robby))

(define license
  '(Apache-2.0 OR MIT))
