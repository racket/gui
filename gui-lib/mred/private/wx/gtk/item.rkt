#lang racket/base
(require ffi/unsafe
         racket/class
         racket/draw/private/local
         (only-in racket/draw make-font)
          "../../syntax.rkt"
         "window.rkt"
         "utils.rkt"
         "types.rkt")

(provide
 (protect-out item%
              install-control-font))

(define _PangoFontDescription _pointer)
(define-gtk gtk_widget_modify_font (_fun _GtkWidget _PangoFontDescription -> _void))

(define (install-control-font gtk font)
  (when font
    (let* ([s (->screen 1)]
    	   [font (if (= s 1)
		     font
		      (make-font #:size (->screen (send font get-size))
				 #:face (send font get-face)
				 #:family (send font get-family)
				 #:style (send font get-style)
				 #:weight (send font get-weight)
				 #:underlined? (send font get-underlined)
				 #:smoothing (send font get-smoothing)
				 #:size-in-pixels? (send font get-size-in-pixels)
				 #:hinting (send font get-hinting)))])
      (gtk_widget_modify_font gtk (send font get-pango)))))

(defclass item% window%
  (inherit get-client-gtk)

  (init-field [callback void])
  (init [font #f])

  (super-new)

  (let ([client-gtk (get-client-gtk)])
    (connect-focus client-gtk)
    (connect-key-and-mouse client-gtk))
  (install-control-font (get-label-gtk) font)
  
  (define/public (get-label-gtk) (get-client-gtk))

  (def/public-unimplemented set-label)
  (def/public-unimplemented get-label)

  (define/public (command e)
    (callback this e)))





