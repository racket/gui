#lang racket/base

(require ffi/unsafe/objc
         racket/draw/private/color
         "types.rkt")

(provide
 color->NSColor)

(import-class NSColor)

(define (color->NSColor c)
  (tell NSColor
        colorWithDeviceRed: #:type _CGFloat (/ (color-red c) 255.0)
        green: #:type _CGFloat (/ (color-green c) 255.0)
        blue: #:type _CGFloat (/ (color-blue c) 255.0)
        alpha: #:type _CGFloat (color-alpha c)))
