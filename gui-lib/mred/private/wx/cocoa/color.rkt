#lang racket/base

(require ffi/unsafe/objc
         racket/draw/private/color
         "types.rkt"
         "utils.rkt")

(provide
 get-default-label-color
 color->NSColor)

(import-class NSColor)

(define (get-default-label-color)
  (if (version-10.10-or-later?)
      (tell NSColor labelColor)
      (tell NSColor controlTextColor)))

(define (color->NSColor c)
  (tell NSColor
        colorWithDeviceRed: #:type _CGFloat (/ (color-red c) 255.0)
        green: #:type _CGFloat (/ (color-green c) 255.0)
        blue: #:type _CGFloat (/ (color-blue c) 255.0)
        alpha: #:type _CGFloat (color-alpha c)))
