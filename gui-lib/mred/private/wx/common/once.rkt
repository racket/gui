#lang racket/base
(require ffi/unsafe
         ffi/unsafe/global)

;; This module must be instantiated only once:
(let ([v (register-process-global #"GRacket-support-initialized" (ptr-add #f 1))])
  (when v
    (error "cannot instantiate `racket/gui/base' a second time in the same process")))
