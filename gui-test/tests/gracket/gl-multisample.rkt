#lang racket/gui
(require ffi/cvector
         sgl/gl
         rackunit)

(define gl-config (new gl-config%))
(send gl-config set-multisample-size 4)

(define f (new frame% [label ""] [width 100] [height 300]))
(define c (new canvas% [parent f] [style '(gl no-autoclear)] [gl-config gl-config]))
(send c with-gl-context
      (λ ()
        (check-equal? (cvector-ref (glGetIntegerv GL_SAMPLE_BUFFERS 1) 0) 1)))
