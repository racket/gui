#lang racket/base
(require ffi/unsafe
	 ffi/unsafe/define
         racket/class
         racket/draw/private/local
         (only-in racket/draw/unsafe/pango
		  pango_cairo_font_map_get_resolution
		  pango_cairo_font_map_get_default)
         (only-in racket/draw make-font)
          "../../syntax.rkt"
         "window.rkt"
         "utils.rkt"
         "types.rkt")

(provide
 (protect-out item%
              install-control-font))

(define _PangoFontDescription _pointer)
(define-gtk gtk_widget_override_font (_fun _GtkWidget _PangoFontDescription -> _void)
  #:make-fail make-not-available)
(define-gtk gtk_widget_modify_font (_fun _GtkWidget _PangoFontDescription -> _void)
  #:fail (lambda () gtk_widget_override_font))

(define (install-control-font gtk font)
  (when font
    (let* ([target-size
	    (cond
	     [gtk3?
	      ;; Gtk3 ignores the "size-in-pixels" part of a
	      ;; font spec, so we have to adjust the text size
	      ;; to compensate.
	      (* (send font get-size)
		 (/ 72.0
		    (pango_cairo_font_map_get_resolution
		     (pango_cairo_font_map_get_default))))]
	     [else (->screen (send font get-size))])]
    	   [font (if (= target-size (send font get-size))
		     font
		      (make-font #:size target-size
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





