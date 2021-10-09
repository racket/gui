#lang info

(define collection 'multi)

(define build-deps '("scheme-lib"
                     "syntax-color-doc"
                     "at-exp-lib"
                     "draw-doc"
                     "draw-lib"
                     "scribble-lib"
                     "snip-lib"
                     "string-constants-lib"
                     "syntax-color-lib"
                     "wxme-lib"
                     "gui-lib"
                     "pict-lib"
                     "racket-doc"
                     "string-constants-doc"
                     "simple-tree-text-markup-doc"
                     "xrepl-doc"))
(define deps '("base"))
(define update-implies '("gui-lib"))

(define pkg-desc "documentation part of \"gui\"")

(define pkg-authors '(mflatt robby))

(define license
  '(Apache-2.0 OR MIT))
