#lang racket/base
(require ffi/unsafe
         ffi/unsafe/global)

;; This module must be instantiated only once:
(let ([v (register-process-global #"GRacket-support-initialized"
                                  (cast 1 _scheme _pointer))])
  (when v
    (error "cannot instantiate `racket/gui/base' a second time in the same process")))
