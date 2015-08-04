#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/alloc
         racket/draw
         racket/draw/private/local
         racket/draw/unsafe/cairo
         "../../lock.rkt"
         racket/draw/unsafe/bstr
         "utils.rkt"
         "types.rkt"
         (only-in '#%foreign ffi-callback))

(provide 
 (protect-out bitmap->pixbuf
              pixbuf->bitmap
              
              _GdkPixbuf
              gtk_image_new_from_pixbuf
              release-pixbuf))

(define _GdkPixbuf (_cpointer/null 'GdkPixbuf))

(define release-pixbuf ((deallocator) g_object_unref))

(define-gtk gtk_image_new_from_pixbuf (_fun _GdkPixbuf -> _GtkWidget))
(define-gdk_pixbuf gdk_pixbuf_new_from_data (_fun _pointer ; data
						  _int ; 0  =RGB
						  _gboolean ; has_alpha?
						  _int ; bits_per_sample
						  _int ; width
						  _int ; height
						  _int ; rowstride
						  _fpointer ; destroy
						  _pointer  ; destroy data
						  -> _GdkPixbuf)
  #:wrap (allocator release-pixbuf))

(define-gdk gdk_cairo_set_source_pixbuf (_fun _cairo_t _GdkPixbuf _double* _double* -> _void))
(define-gdk_pixbuf gdk_pixbuf_get_width (_fun _GdkPixbuf -> _int))
(define-gdk_pixbuf gdk_pixbuf_get_height (_fun _GdkPixbuf -> _int))

(define free-it (ffi-callback free
                              (list _pointer)
                              _void
                              #f
                              #t))

(define (bitmap->pixbuf orig-bm [scale 1.0])
  (let* ([w (send orig-bm get-width)]
         [h (send orig-bm get-height)]
	 [sw (ceiling (inexact->exact (* scale w)))]
         [sh (ceiling (inexact->exact (* scale h)))]
         [str (make-bytes (* sw sh 4) 255)])
    (define-values (bm unscaled? usw ush)
      (cond
       [(= scale 1.0) (values orig-bm #f w h)]
       [(= scale (send orig-bm get-backing-scale)) (values orig-bm #t sw sh)]
       [else (values (rescale orig-bm scale) #f sw sh)]))
    (send bm get-argb-pixels 0 0 usw ush str #f #:unscaled? unscaled?)
    (let ([mask (send bm get-loaded-mask)])
      (when mask
        (send mask get-argb-pixels 0 0 usw ush str #t #:unscaled? unscaled?)))
    (atomically
     (let ([rgba (scheme_make_sized_byte_string (malloc (* sw sh 4) 'raw) (* sw sh 4) 0)])
       (memcpy rgba (ptr-add str 1) (sub1 (* sw sh 4)))
       (for ([i (in-range 0 (* sw sh 4) 4)])
         (bytes-set! rgba (+ i 3) (bytes-ref str i)))
       (gdk_pixbuf_new_from_data rgba
                                 0
                                 #t
                                 8
                                 sw
                                 sh
                                 (* sw 4)
                                 free-it
                                 #f)))))

(define (pixbuf->bitmap pixbuf)
  (let* ([w (gdk_pixbuf_get_width pixbuf)]
         [h (gdk_pixbuf_get_height pixbuf)]
         [bm (make-object bitmap% w h #f #t)]
         [s (send bm get-cairo-surface)]
         [cr (cairo_create s)])
    (gdk_cairo_set_source_pixbuf cr pixbuf 0 0)
    (cairo_rectangle cr 0 0 w h)
    (cairo_fill cr)
    (cairo_destroy cr)
    bm))

(define (rescale bm scale)
  (define w (send bm get-width))
  (define h (send bm get-height))
  (define new-bm (make-bitmap (ceiling (inexact->exact (* scale w)))
			      (ceiling (inexact->exact (* scale h)))))
  (define dc (send new-bm make-dc))
  (send dc set-scale scale scale)
  (send dc set-smoothing 'smoothed)
  (send dc draw-bitmap bm 0 0)
  new-bm)
