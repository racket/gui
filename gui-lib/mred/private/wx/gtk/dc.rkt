#lang racket/base
(require ffi/unsafe
	 ffi/unsafe/define
         racket/class
         "utils.rkt"
         "types.rkt"
         "window.rkt"
         "frame.rkt"
         "x11.rkt"
         "win32.rkt"
         "gl-context.rkt"
	 "../../lock.rkt"
         "../common/backing-dc.rkt"
         racket/draw/unsafe/cairo
         racket/draw/private/dc
         racket/draw/private/bitmap
         racket/draw/private/local
         ffi/unsafe/alloc)

(provide 
 (protect-out dc%
              do-backing-flush
              x11-bitmap%

              gdk_gc_new
              gdk_gc_unref
              gdk_gc_set_rgb_fg_color
              gdk_gc_set_line_attributes
              gdk_draw_rectangle))

(define-gdk gdk_cairo_create (_fun _pointer -> _cairo_t)
  #:wrap (allocator cairo_destroy))

(define-gdk gdk_gc_unref (_fun _pointer -> _void)
  #:wrap (deallocator)
  #:make-fail make-not-available)
(define-gdk gdk_gc_new (_fun _GdkWindow -> _pointer)
  #:wrap (allocator gdk_gc_unref)
  #:make-fail make-not-available)
(define-gdk gdk_gc_set_rgb_fg_color (_fun _pointer _GdkColor-pointer -> _void)
  #:make-fail make-not-available)
(define-gdk gdk_gc_set_line_attributes (_fun _pointer _int _int _int _int -> _void)
  #:make-fail make-not-available)
(define-gdk gdk_draw_rectangle (_fun _GdkWindow _pointer _gboolean _int _int _int _int -> _void)
  #:make-fail make-not-available)

(define-cstruct _GdkVisual-rec ([type-instance _pointer]
				[ref_count _uint]
				[qdata _pointer]
				[type _int]
				[depth _int]))
(define-gdk gdk_visual_get_system (_fun -> _GdkVisual-rec-pointer))

(define x11-bitmap%
  (class bitmap%
    (init w h gtk)

    (define sf
      (if gtk3?
	  (if gtk
	      (->screen (gtk_widget_get_scale_factor gtk))
	      (display-bitmap-resolution 0 (lambda () 1.0)))
	  (->screen 1.0)))
    (define/private (scale x)
      (min (max 1 (ceiling (inexact->exact (* sf x)))) 32000))

    (define-values (pixmap xdisplay xvisual)
      (let ([gdk-win (and gtk (widget-window gtk))])
	(if gtk3?
	    (let* ([gdk-win (or gdk-win
				(gdk_screen_get_root_window
				 (gdk_screen_get_default)))]
		   [xdisplay (gdk_x11_display_get_xdisplay
			      (if gdk-win
				  (gdk_window_get_display gdk-win)
				  (gdk_display_get_default)))]
		   [visual (gdk_window_get_visual gdk-win)])
	      ;; We must not get here for a transparent canvas,
	      ;; because getting an XID will force a native window.
	      (values (XCreatePixmap xdisplay
				     (gdk_x11_window_get_xid gdk-win)
				     (scale w) (scale h)
				     (gdk_visual_get_depth visual))
		      xdisplay
		      (gdk_x11_visual_get_xvisual visual)))
	    (let ([pixmap (gdk_pixmap_new gdk-win
					  (scale w)
					  (scale h)
					  (if gdk-win 
					      -1
					      (GdkVisual-rec-depth
					       (gdk_visual_get_system))))])
	      (values pixmap
		      (gdk_x11_display_get_xdisplay
		       (gdk_drawable_get_display pixmap))
		      (gdk_x11_visual_get_xvisual
		       (gdk_drawable_get_visual pixmap)))))))

    (define s
      (cairo_xlib_surface_create xdisplay
                                 (if gtk3?
				     (cast pixmap _Pixmap _ulong)
				     (gdk_x11_drawable_get_xid pixmap))
				 xvisual
                                 (scale w)
                                 (scale h)))

    (define gl #f)

    (super-make-object (make-alternate-bitmap-kind
			w
			h
			sf))

    ;; initialize bitmap to white:
    (let ([cr (cairo_create s)])
      (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
      (cairo_paint cr)
      (cairo_destroy cr))

    ;; `get-gdk-pixmap' and `install-gl-context' are
    ;; localized in "gl-context.rkt"
    (define/public (get-gdk-pixmap) pixmap)
    (define/public (install-gl-context new-gl) (set! gl new-gl))

    (define/override (get-bitmap-gl-context) gl)

    (define/override (ok?) #t)
    (define/override (is-color?) #t)
    (define/override (has-alpha-channel?) #f)
    
    (define/override (get-cairo-surface) s)

    (define/override (release-bitmap-storage)
      (atomically
       (cairo_surface_destroy s)
       (if gtk3?
	   (XFreePixmap xdisplay pixmap)
	   (gobject-unref pixmap))
       (set! s #f)))))

(define cairo-bitmap%
  (class bitmap%
    (init w h gtk)
    (super-make-object w h #f #t
		       (if gtk3?
			   (if gtk
			       (->screen (gtk_widget_get_scale_factor gtk))
			       (display-bitmap-resolution 0 (lambda () 1.0)))
			   (->screen 1.0)))))

(define win32-bitmap%
  (class bitmap%
    (init w h gdk-win)
    (super-make-object (make-alternate-bitmap-kind w h))

    (define s
      (if (not gdk-win)
	  (cairo_win32_surface_create_with_dib CAIRO_FORMAT_RGB24 w h)
	  (atomically
	   (let ([hdc (GetDC (gdk_win32_drawable_get_handle gdk-win))])
	     (begin0
	      (cairo_win32_surface_create_with_ddb hdc
						   CAIRO_FORMAT_RGB24 w h)
	      (ReleaseDC hdc))))))

    (define/override (ok?) #t)
    (define/override (is-color?) #t)
    (define/override (has-alpha-channel?) #f)

    (define/override (get-cairo-surface) s)

    (define/override (release-bitmap-storage)
      (atomically
       (cairo_surface_destroy s)
       (set! s #f)))))

(define dc%
  (class backing-dc%
    (init [(cnvs canvas)]
          transparentish?)
    (inherit end-delay)
    (define canvas cnvs)
    (define gl #f)
    (define is-transparentish? transparentish?)

    (super-new [transparent? transparentish?])

    (define/override (get-gl-context)
      (or gl
          (let ([v (create-widget-gl-context (send canvas get-client-gtk))])
	    (when v (set! gl v))
	    v)))

    (define/override (make-backing-bitmap w h)
      (cond
       [(and (not is-transparentish?)
             (eq? 'unix (system-type)))
	(make-object x11-bitmap% w h (send canvas get-client-gtk))]
       [(and (not is-transparentish?)
             (eq? 'windows (system-type)))
	(make-object win32-bitmap% w h (widget-window (send canvas get-client-gtk)))]
       [else
	;; Transparent canvas always use a Cairo bitmap:
	(make-object cairo-bitmap% (max 1 w) (max 1 h) (send canvas get-client-gtk))]))

    (define/override (get-backing-size xb yb)
      (send canvas get-client-size xb yb))

    (define/override (get-size)
      (let ([xb (box 0)]
            [yb (box 0)])
        (send canvas get-virtual-size xb yb)
        (values (unbox xb) (unbox yb))))

    (define/override (queue-backing-flush)
      ;; Re-enable expose events so that the queued 
      ;; backing flush will be handled:
      (end-delay)
      (send canvas queue-backing-flush))

    (define/override (flush)
      (send canvas flush))

    (define/override (request-delay)
      (request-flush-delay (send canvas get-flush-window) is-transparentish?))
    (define/override (cancel-delay req)
      (cancel-flush-delay req))))

(define (do-backing-flush canvas dc win-or-cr)
  (send dc on-backing-flush
        (lambda (bm)
          (let ([w (box 0)]
                [h (box 0)])
            (send canvas get-client-size w h)
            (let ([cr (if gtk3?
			  win-or-cr
			  (gdk_cairo_create win-or-cr))])
	      (cairo_scale cr (->screen 1.0) (->screen 1.0))
              (backing-draw-bm bm cr (unbox w) (unbox h) 0 0 (->screen 1.0))
	      (unless gtk3?
                (cairo_destroy cr)))))))
