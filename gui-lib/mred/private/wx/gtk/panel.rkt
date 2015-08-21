#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/define
         racket/draw/unsafe/cairo
          "../../syntax.rkt"
          "../../lock.rkt"
         "window.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "dc.rkt")

(provide 
 (protect-out panel%
              panel-mixin
              panel-container-mixin

              gtk_fixed_new
              gtk_fixed_move
              gtk_event_box_new

              gtk_container_set_border_width
              connect-expose/draw-border))

(define-gtk gtk_fixed_new (_fun -> _GtkWidget))
(define-gtk gtk_event_box_new (_fun -> _GtkWidget))
(define-gtk gtk_event_box_set_visible_window (_fun _GtkWidget _gboolean -> _void))

(define-gtk gtk_fixed_move (_fun _GtkWidget _GtkWidget _int _int -> _void))

(define-gtk gtk_container_set_border_width (_fun _GtkWidget _int -> _void))

(define-signal-handler connect-expose-border "expose-event"
  (_fun _GtkWidget _GdkEventExpose-pointer -> _gboolean)
  (lambda (gtk event)
    (let* ([win (widget-window gtk)]
           [gc (gdk_gc_new win)]
           [gray #x8000])
      (when gc
        (gdk_gc_set_rgb_fg_color gc (make-GdkColor 0 gray gray gray))
	(unless (= 1 (->screen 1))
	  (gdk_gc_set_line_attributes gc (->screen 1) 0 0 0))
        (let* ([a (widget-allocation gtk)]
               [w (sub1 (GtkAllocation-width a))]
               [h (sub1 (GtkAllocation-height a))])
          (let loop ([gtk gtk] [x 0] [y 0] [can-super? #t])
            (if (and can-super?
		     (not (gtk_widget_get_has_window gtk)))
                ;; no window:
                (let ([a (widget-allocation gtk)])
                  (loop (widget-parent gtk) (+ x (GtkAllocation-x a)) (+ y (GtkAllocation-y a))
			;; It seems that a widget's allocation is with respect
			;; to a window, not its parent.
			#f))
                ;; found window:
                (gdk_draw_rectangle win gc #f x y w h))))
        (gdk_gc_unref gc)))
    #f))

(define-gtk gtk_widget_get_allocated_width (_fun _GtkWidget -> _int)
  #:make-fail make-not-available)
(define-gtk gtk_widget_get_allocated_height (_fun _GtkWidget -> _int)
  #:make-fail make-not-available)

(define-signal-handler connect-draw-border "draw"
  (_fun _GtkWidget _cairo_t -> _gboolean)
  (lambda (gtk cr)
    (cairo_set_source_rgba cr 0.5 0.5 0.5 1.0)
    (cairo_set_line_width cr 1.0)
    (cairo_rectangle cr
		     0.5 0.5
		     (- (gtk_widget_get_allocated_width gtk) 1)
		     (- (gtk_widget_get_allocated_height gtk) 1))
    (cairo_stroke cr)
    #f))

(define (connect-expose/draw-border gtk border-gtk)
  (if gtk3?
      (connect-draw-border gtk #:after? #t)
      (connect-expose-border border-gtk)))

(define (panel-mixin %)
  (class %

    (define lbl-pos 'horizontal)
    (define children null)

    (super-new)

    (define/public (get-label-position) lbl-pos)
    (define/public (set-label-position pos) (set! lbl-pos pos))

    (define/public (adopt-child child)
      ;; in atomic mode
      (send child set-parent this))

    (define/override (reset-child-freezes)
      (super reset-child-freezes)
      (when (pair? children)
        (for ([child (in-list children)])
          (send child reset-child-freezes))))

    (define/override (reset-child-dcs)
      (super reset-child-dcs)
      (when (pair? children)
        (for ([child (in-list children)])
          (send child reset-child-dcs))))
    
    (define/override (paint-children)
      (super paint-children)
      (when (pair? children)
        (for ([child (in-list children)])
          (send child paint-children))))
    
    (define/override (set-size x y w h)
      (super set-size x y w h)
      (reset-child-dcs))

    (define/override (register-child child on?)
      (let ([now-on? (and (memq child children) #t)])
        (unless (eq? on? now-on?)
          (set! children 
                (if on?
                    (cons child children)
                    (remq child children))))))

    (define/override (refresh-all-children)
      (for ([child (in-list children)])
        (send child refresh)))

    (define/public (set-item-cursor x y) (void))))

(define (panel-container-mixin %)
  (class %
    (inherit get-container-gtk)
    (super-new)
    (define/override (set-child-size child-gtk x y w h)
      (gtk_fixed_move (get-container-gtk) child-gtk (->screen x) (->screen y))
      (gtk_widget_set_size_request child-gtk (->screen w) (->screen h)))))

(define-gdk gdk_window_has_native (_fun _GdkWindow -> _gboolean))

(define panel%
  (class (panel-container-mixin (panel-mixin window%))
    (init parent
          x y w h
          style
          label)
    
    (inherit get-gtk set-auto-size set-size
             adjust-client-delta)

    ;; With GTK+ 3, an event box draws solid over
    ;; the background, which interferes with themes
    ;; can controls that have their own background.
    ;; The gtk_event_box_set_visible_window function
    ;; avoids that, but ensure that no child forces
    ;; it to be a native window at the GDK level.
    ;; In particular, scrolls force the enclosing
    ;; parent to have a native window, so add a layer
    ;; as needed around scrolls. Also, for tab panels,
    ;; a non-hidden event box seems to be needed around
    ;; the panel to deliver events to the tab.
    (define gtk (as-gtk-allocation (gtk_event_box_new)))
    (when gtk3?
      (gtk_event_box_set_visible_window gtk #f))
    (define border-gtk (atomically
                        (and (memq 'border style)
                             (let ([border-gtk (gtk_fixed_new)])
                               (gtk_container_add gtk border-gtk)
                               (gtk_container_set_border_width border-gtk 1)
                               (connect-expose/draw-border gtk border-gtk)
                               (gtk_widget_show border-gtk)
                               border-gtk))))
    (define client-gtk (atomically
                        (let ([client (gtk_fixed_new)])
                          (gtk_container_add (or border-gtk gtk) client)
                          (gtk_widget_show client)
                          client)))
    
    (define/override (get-client-gtk) client-gtk)

    (define/override (gets-focus?) #f)

    (super-new [parent parent]
               [gtk gtk]
               [extra-gtks (list client-gtk)]
               [no-show? (memq 'deleted style)])

    ;; Start with a minimum size:
    (set-size 0 0 (if border-gtk 3 1) (if border-gtk 3 1))
    (when border-gtk
      (adjust-client-delta 2 2))
    
    (connect-key-and-mouse gtk)
    (gtk_widget_add_events gtk (bitwise-ior GDK_BUTTON_PRESS_MASK
                                            GDK_BUTTON_RELEASE_MASK
                                            GDK_POINTER_MOTION_HINT_MASK
                                            GDK_FOCUS_CHANGE_MASK
                                            GDK_ENTER_NOTIFY_MASK
                                            GDK_LEAVE_NOTIFY_MASK))))
