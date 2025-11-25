#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         "utils.rkt"
         "types.rkt"
         "const.rkt")

(provide liquid-glass?)

(define liquid-glass?
  (and (version-26.0-or-later?)
       (let ()
         (import-class NSWindow)
         (objc-class-has-instance-method? NSWindow (selector _cornerRadius))
         ;; This seems like a terrible way to detect whether we're using
         ;; the GUI rendering introduced in Tahoe (it depends on how Racket
         ;; is linked, not the OS it runs on), but this is the best
         ;; recommendation I can find for now:
         (define cocoa
           (tell (tell NSWindow alloc)
                 initWithContentRect: #:type _NSRect (make-NSRect (make-NSPoint 0 0)
                                                                  (make-NSSize 256 256))
                 styleMask: #:type _int NSTitledWindowMask
                 backing: #:type _int NSBackingStoreBuffered
                 defer: #:type _BOOL NO))
         ((tell #:type _double cocoa _cornerRadius) . > . 12.0))))

