#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         "utils.rkt")

(provide 
 (protect-out define-x11

	      gdk_pixmap_new
              gdk_window_get_display
              gdk_drawable_get_display
              gdk_window_get_visual
              gdk_drawable_get_visual
	      gdk_visual_get_best
	      gdk_screen_get_root_window
	      gdk_visual_get_depth
              gdk_x11_drawable_get_xid
              gdk_x11_display_get_xdisplay
              gdk_x11_visual_get_xvisual
              gdk_x11_screen_get_screen_number
	      gdk_x11_window_get_xid

	      _Display
	      _Window
	      _Pixmap
	      XCreatePixmap
	      XFreePixmap
	      XCreateSimpleWindow
	      XDestroyWindow))

(define x11-lib (ffi-lib "libX11" '("6" "5" "")))

(define-ffi-definer define-x11 x11-lib
  #:default-make-fail make-not-available)

(define _GdkDrawable _pointer)
(define _GdkDisplay (_cpointer 'GdkDisplay))
(define _GdkWindow (_cpointer 'GdkWindow))
(define _GdkScreen (_cpointer 'GdkScreen))
(define _GdkVisual (_cpointer 'GdkVisual))
(define _GdkPixmap (_cpointer 'GdkPixmap))
(define _Visual (_cpointer 'Visual))
(define _Display (_cpointer 'Display))
(define _Drawable _ulong)

;; This should be `_ulong`, but we use pointers for various
;; reasons, including support for dealloctaors:
(define _Window (_cpointer 'Window))
(define _Pixmap (_cpointer 'Pixmap))

(define-gdk gdk_pixmap_new (_fun _GdkDrawable _int _int _int -> _GdkPixmap)
  #:wrap (allocator gobject-unref)
  #:make-fail make-not-available)

(define-gdk gdk_drawable_get_display (_fun _GdkDrawable -> _GdkDisplay)
  #:make-fail make-not-available)
(define-gdk gdk_window_get_display (_fun _GdkWindow -> _GdkDisplay)
  #:make-fail make-not-available)
(define-gdk gdk_drawable_get_visual (_fun _GdkDrawable -> _GdkVisual)
  #:make-fail make-not-available)

(define-gdk gdk_visual_get_best (_fun -> _GdkVisual)
  #:make-fail make-not-available)

(define-gdk gdk_window_get_visual (_fun _GdkWindow -> _GdkVisual)
  #:make-fail make-not-available)
(define-gdk gdk_visual_get_depth (_fun _GdkVisual -> _int)
  #:make-fail make-not-available)

(define-gdk gdk_screen_get_root_window (_fun _GdkScreen -> _GdkWindow))

(define-gtk gdk_x11_window_get_xid (_fun _GdkWindow -> _Window)
  #:make-fail make-not-available)
(define-gdk gdk_x11_drawable_get_xid (_fun _GdkDrawable -> _Drawable)
  #:fail (lambda () (lambda (d)
		      (cast
		       (gdk_x11_window_get_xid (cast d _GdkDrawable _GdkWindow))
		       _pointer
		       _ulong))))

(define-gdk gdk_x11_display_get_xdisplay (_fun _GdkDisplay -> _Display)
  #:make-fail make-not-available)

(define-gdk gdk_x11_visual_get_xvisual (_fun _GdkVisual -> _Visual)
  #:make-fail make-not-available)

(define-gdk gdk_x11_screen_get_screen_number (_fun _GdkScreen -> _int)
  #:make-fail make-not-available)

(define-x11 XFreePixmap (_fun _Display _Pixmap -> _void)
  #:wrap (deallocator cadr))
(define-x11 XCreatePixmap (_fun _Display _Window _int _int _int -> _Pixmap)
  #:wrap (lambda (proc)
	   (lambda (dpy win w h d)
	     (((allocator (lambda (pixmap)
			    (XFreePixmap dpy pixmap)))
	       (lambda ()
		 (proc dpy win w h d)))))))

;; No finalization here, because we rely on destroying the
;; enclosing window to release a created window, if
;; necessary.
(define-x11 XDestroyWindow (_fun _Display _Window -> _void))
(define-x11 XCreateSimpleWindow (_fun _Display _Window
				      _int _int _int _int
				      _int _long _long
				      -> _Window))

