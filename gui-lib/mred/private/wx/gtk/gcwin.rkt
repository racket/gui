#lang racket/base
(require ffi/unsafe
	 ffi/unsafe/define
	 racket/draw/unsafe/cairo
	 racket/class
         "utils.rkt"
         "types.rkt"
         "window.rkt"
	 "queue.rkt"
         "pixbuf.rkt"
	 "x11.rkt")

(provide 
 (protect-out scheme_add_gc_callback
              scheme_remove_gc_callback
              create-gc-window
              free-gc-window
              make-gc-show-desc
              make-gc-hide-desc
	      bitmap->gc-bitmap))

;; Gtk2, only:
(define-cstruct _GdkWindowAttr2
  ([title _string]
   [event_mask _int]
   [x _int]
   [y _int]
   [width _int]
   [height _int]
   [wclass _int] ; GDK_INPUT_OUTPUT
   [visual _pointer]
   [colormap _pointer] ; this field is absent in Gtk3
   [window_type _int] ; GDK_WINDOW_CHILD
   [cursor _pointer]
   [wmclass_name _string]
   [wmclass_class _string]
   [override_redirect _gboolean]
   [type_hint _int]))

;; Gtk3, only:
(define-cstruct _GdkWindowAttr3
  ([title _string]
   [event_mask _int]
   [x _int]
   [y _int]
   [width _int]
   [height _int]
   [wclass _int] ; GDK_INPUT_OUTPUT
   [visual _pointer]
   [window_type _int] ; GDK_WINDOW_CHILD
   [cursor _pointer]
   [wmclass_name _string]
   [wmclass_class _string]
   [override_redirect _gboolean]
   [type_hint _int]))

(define make-GdkWindowAttr
  (if gtk3?
      (lambda (t e x y w h wc vis cm wt c wmc_n wmc_c o th)
	(make-GdkWindowAttr3 t e x y w h wc vis wt c wmc_n wmc_c o th))
      make-GdkWindowAttr2))

(define << arithmetic-shift)

(define GDK_WA_TITLE (1 . << . 1))
(define GDK_WA_X (1 . << . 2))
(define GDK_WA_Y (1 . << . 3))
(define GDK_WA_CURSOR (1 . << . 4))
(define GDK_WA_COLORMAP (1 . << . 5))
(define GDK_WA_VISUAL (1 . << . 6))
(define GDK_WA_WMCLASS (1 . << . 7))
(define GDK_WA_NOREDIR (1 . << . 8))
(define GDK_WA_TYPE_HINT (1 . << . 9))

(define GDK_INPUT_OUTPUT 0)

(define GDK_WINDOW_CHILD 2)

(define-gdk gdk_window_new (_fun _GdkWindow
				 (if gtk3?
				     _GdkWindowAttr3-pointer
				     _GdkWindowAttr2-pointer)
				 _uint -> _GdkWindow))

(define-gdk gdk_window_show-p _fpointer
  #:c-id gdk_window_show)
(define-gdk gdk_window_hide _fpointer)
(define-gdk gdk_display_flush _fpointer)

(define-gdk gdk_window_show (_fun _GdkWindow -> _void))

;; Gtk2
(define-gdk gdk_draw_pixbuf _fpointer
  #:make-fail make-not-available)

(define-mz scheme_add_gc_callback (_fun _racket _racket -> _racket))
(define-mz scheme_remove_gc_callback (_fun _racket -> _void))

(define-x11 XSetWindowBackgroundPixmap _fpointer #:fail (lambda () #f))
(define-x11 XMapRaised _fpointer #:fail (lambda () #f))
(define-x11 XUnmapWindow _fpointer #:fail (lambda () #f))

(define use-x11? (and gtk3? (not wayland?)))

(define (bitmap->gc-bitmap bm client-gtk)
  (cond
   [use-x11?
    ; Generate an X11 Pixmap
    (define gwin (widget-window client-gtk))
    (define display (gdk_x11_display_get_xdisplay (gdk_window_get_display gwin)))
    (define sf (->screen (gtk_widget_get_scale_factor client-gtk)))
    (define w (send bm get-width))
    (define h (send bm get-height))
    (define bms (send bm get-backing-scale))
    (define cw (inexact->exact (ceiling (* sf w))))
    (define ch (inexact->exact (ceiling (* sf h))))
    (define visual (gdk_window_get_visual gwin))
    (define pixmap (XCreatePixmap display
				  (gdk_x11_window_get_xid gwin)
				  cw ch
				  (gdk_visual_get_depth visual)))
    (define s (cairo_xlib_surface_create display
					 (cast pixmap _pointer _ulong)
					 (gdk_x11_visual_get_xvisual visual)
					 cw ch))
    (define cr (cairo_create s))
    (define pat (cairo_pattern_create_for_surface (send bm get-handle)))
    (cairo_pattern_set_matrix pat (make-cairo_matrix_t (/ bms sf) 0.0
						       0.0 (/ bms sf)
						       0.0 0.0))
    (cairo_set_source cr pat)
    (cairo_pattern_destroy pat)
    (cairo_rectangle cr 0 0 cw ch)
    (cairo_fill cr)
    (cairo_destroy cr)
    (cairo_surface_destroy s)
    pixmap]
   [else
    ;; Generate a Gdk Pixbuf
    (bitmap->pixbuf bm (->screen 1.0))]))

(define (create-gc-window client-gtk x y w h)
  (define cwin (widget-window client-gtk))
  (cond
   [use-x11?
    ;; Work at the level of X11 to change the screen without an event loop
    (define display (gdk_x11_display_get_xdisplay (gdk_window_get_display cwin)))
    (define s (gtk_widget_get_scale_factor client-gtk))
    (cons display
	  (XCreateSimpleWindow display
			       (gdk_x11_window_get_xid cwin)
			       (* s x) (* s y) (* s w) (* s h) 0 0 0))]
   [else
    (gdk_window_new cwin (make-GdkWindowAttr
			  ""
			  0
			  x y w h
			  GDK_INPUT_OUTPUT
			  #f #f
			  GDK_WINDOW_CHILD
			  #f
			  "" "" #f 0)
		    (bitwise-ior GDK_WA_X
				 GDK_WA_Y))]))

(define (free-gc-window win)
  (cond
   [use-x11? (XDestroyWindow (car win) (cdr win))]
   [else (g_object_unref win)]))

(define (make-draw win gc-bitmap w h)
  (cond
   [use-x11? (vector
	      (vector 'ptr_ptr_ptr->void
		      XSetWindowBackgroundPixmap
		      (car win)
		      (cdr win)
		      gc-bitmap))]
   [gtk3? (vector)]
   [else (vector
	  (vector 'ptr_ptr_ptr_int_int_int_int_int_int_int_int_int->void
		  gdk_draw_pixbuf
		  win #f gc-bitmap
		  0 0 0 0 w h
		  0 0 0))]))

(define (make-flush)
  (vector
   (vector 'ptr_ptr_ptr->void gdk_display_flush (gdk_display_get_default) #f #f)))

(define (vector* . l)
  (for*/vector ([v (in-list l)] [e (in-vector v)]) e))

(define (make-gc-show-desc win gc-bitmap w h)
  (cond
   [use-x11? (vector*
	      (make-draw win gc-bitmap w h)
	      (vector
	       (vector 'ptr_ptr_ptr->void
		       XMapRaised
		       (car win)
		       (cdr win)
		       #f))
	      (make-flush))]
   [else  (vector*
	   (vector
	    (vector 'ptr_ptr_ptr->void gdk_window_show-p win #f #f))
	   (make-draw win gc-bitmap w h)
	   (make-flush))]))

(define (make-gc-hide-desc win gc-bitmap w h)
  (vector*
   ;; draw the ``off'' bitmap so we can flush immediately
   (make-draw win gc-bitmap w h)
   (make-flush)
   (vector
    ;; hide the window; it may take a while for the underlying canvas
    ;; to refresh:
    (if use-x11?
	(vector 'ptr_ptr_ptr->void
		XUnmapWindow
		(car win)
		(cast (cdr win) _Window _pointer)
		#f)
	(vector 'ptr_ptr_ptr->void gdk_window_hide win #f #f)))))
