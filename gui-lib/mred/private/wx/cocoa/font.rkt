#lang racket/base
(require racket/class
         racket/draw
         ffi/unsafe
         ffi/unsafe/objc
         ffi/unsafe/alloc
         racket/draw/unsafe/pango
         racket/draw/private/dc
         "../../lock.rkt"
         "const.rkt"
         "utils.rkt"
         "types.rkt")

(provide 
 (protect-out font->NSFont)
 system-control-font-name)

(import-class NSFont NSFontManager)

(define NSItalicFontMask #x00000001)
(define NSBoldFontMask #x00000002)

(define (font->NSFont f)
  (let* ([weight (send f get-weight)]
         [style (send f get-style)]
         [name (or (send f get-face)
                   (send the-font-name-directory
                         get-screen-name
                         (send the-font-name-directory
                               find-family-default-font-id
                               (send f get-family))
                         weight
                         style))]
         [name (regexp-replace #rx",.*" name "")])
    (atomically
     (with-autorelease
      (let ([f (tell NSFont 
                     fontWithName: #:type _NSString name
                     size: #:type _CGFloat (send f get-point-size))])
        (if (and (eq? 'normal weight)
                 (eq? 'normal style))
            (begin
              (retain f)
              f)
            (let ([fm (tell NSFontManager sharedFontManager)])
              (let ([f (tell fm
                             convertFont: f 
                             toHaveTrait: #:type _int (bitwise-ior
                                                       (if (eq? weight 'bold) NSBoldFontMask 0)
                                                       (if (eq? style 'italic) NSItalicFontMask 0)))])
                (begin
                  (retain f)
                  f)))))))))

;; ------------------------------------------------------------

;; As of OS X 10.11, the font that is used for controls is not
;; accessible through a font family. (The idea is that the face can
;; vary with the requested size --- but we don't want to go there in
;; general.) To make the main face accessible, we've added a hook to
;; Pango to register a family manually, mapping the family name to one
;; or more face descriptions.

(define-cocoa kCTFontFamilyNameAttribute _pointer)
(define-cocoa CTFontDescriptorCopyAttribute (_fun _pointer _pointer -> _NSString))
(define-cf CFRelease (_fun _pointer -> _void)
  #:wrap (deallocator))
(define-cocoa CTFontDescriptorCreateWithNameAndSize (_fun _NSString _CGFloat -> _pointer)
  #:wrap (allocator CFRelease))
  
(define system-control-font-name
  (and (version-10.11-or-later?)
       (with-autorelease
           (let ([control-font (tell NSFont systemFontOfSize: #:type _double 13.0)])
             (and control-font
                  (let ([desc (tell #:type _pointer control-font fontDescriptor)])
                    (and desc
                         (CTFontDescriptorCopyAttribute desc kCTFontFamilyNameAttribute))))))))

(when system-control-font-name
  (set-font-map-init-hook!
   (lambda (fm)
     (define n-desc (CTFontDescriptorCreateWithNameAndSize system-control-font-name 0.0))
     (pango_core_text_add_family_for_font_descriptors fm system-control-font-name 1 (vector n-desc))
     (CFRelease n-desc))))
