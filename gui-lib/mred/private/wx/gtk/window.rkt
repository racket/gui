#lang racket/base
(require ffi/unsafe
	 ffi/unsafe/define
         racket/class
         net/uri-codec
         ffi/unsafe/atomic
         "../../syntax.rkt"
         "../../lock.rkt"
         "../common/event.rkt"
         "../common/freeze.rkt"
         "../common/queue.rkt"
         "../common/local.rkt"
         "../common/delay.rkt"
         racket/draw/unsafe/bstr
         "keycode.rkt"
         "keymap.rkt"
         "queue.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt"
         "widget.rkt"
         "clipboard.rkt")

(provide 
 (protect-out window%
              queue-window-event
              queue-window-refresh-event

              gtk_widget_realize 
              gtk_container_add
              gtk_widget_add_events
              gtk_widget_size_request
              gtk_widget_set_size_request
	      gtk_widget_size_allocate
	      gtk_widget_get_preferred_size
              gtk_widget_grab_focus
              gtk_widget_has_focus
	      gtk_widget_get_mapped
	      gtk_widget_get_has_window
	      gtk_widget_set_can_default
	      gtk_widget_set_can_focus
              gtk_widget_set_sensitive
	      gtk_widget_get_scale_factor

              connect-focus
              connect-key
              connect-key-and-mouse
              connect-enter-and-leave
              do-button-event

              (struct-out GtkRequisition) _GtkRequisition-pointer
              (struct-out GtkAllocation) _GtkAllocation-pointer

              widget-window
              widget-allocation
              widget-parent

              the-accelerator-group
              gtk_window_add_accel_group
              gtk_menu_set_accel_group

              flush-display
              gdk_display_get_default

              request-flush-delay
              cancel-flush-delay
              win-box-valid?
              window->win-box
              unrealize-win-box)
 gtk->wx
 gtk_widget_show
 gtk_widget_hide)

;; ----------------------------------------

(define-gtk gtk_container_add (_fun _GtkWidget _GtkWidget -> _void))
(define-gtk gtk_container_remove (_fun _GtkWidget _GtkWidget -> _void))
(define-gtk gtk_widget_realize (_fun _GtkWidget -> _void))
(define-gtk gtk_widget_add_events (_fun _GtkWidget _int -> _void))

(define-gdk gdk_keyval_to_unicode (_fun _uint -> _uint32))

(define-gtk gtk_widget_get_display (_fun _GtkWidget -> _GdkDisplay))
(define-gtk gtk_widget_get_screen (_fun _GtkWidget -> _GdkScreen))
(define-gdk gdk_display_warp_pointer (_fun _GdkDisplay _GdkScreen _int _int -> _void))

(define-cstruct _GtkRequisition ([width _int]
                                 [height _int]))
(define-cstruct _GtkAllocation ([x _int]
                                [y _int]
                                [width _int]
                                [height _int]))

(define-gtk gtk_widget_size_request (_fun _GtkWidget _GtkRequisition-pointer -> _void))
(define-gtk gtk_widget_size_allocate (_fun _GtkWidget _GtkAllocation-pointer -> _void))
(define-gtk gtk_widget_set_size_request (_fun _GtkWidget _int _int -> _void))
(define-gtk gtk_widget_grab_focus (_fun _GtkWidget -> _void))
(define-gtk gtk_widget_is_focus (_fun _GtkWidget -> _gboolean))
(define-gtk gtk_widget_set_sensitive (_fun _GtkWidget _gboolean -> _void))
(define-gtk gtk_widget_get_preferred_size (_fun _GtkWidget _GtkRequisition-pointer/null _GtkRequisition-pointer/null -> _void)
  #:fail (lambda () #f))
(define-gtk gtk_widget_get_scale_factor (_fun _GtkWidget -> _int)
  #:fail (lambda () (lambda (gtk) 1)))

(define-gdk gdk_keyboard_grab (_fun _GdkWindow _gboolean _int -> _void))
(define-gdk gdk_keyboard_ungrab (_fun _int -> _void))

(define _GtkAccelGroup (_cpointer 'GtkAccelGroup))
(define-gtk gtk_accel_group_new (_fun -> _GtkAccelGroup))
(define-gtk gtk_window_add_accel_group (_fun _GtkWindow _GtkAccelGroup -> _void))
(define-gtk gtk_menu_set_accel_group (_fun _GtkWidget _GtkAccelGroup -> _void))

(define the-accelerator-group (gtk_accel_group_new))

;; Only for Gtk2
(define-cstruct _GtkWidgetT ([obj _GtkObject]
                             [private_flags _uint16]
                             [state _byte]
                             [saved_state _byte]
                             [name _pointer]
                             [style _pointer]
                             [req _GtkRequisition]
                             [alloc _GtkAllocation]
                             [window _GdkWindow]
                             [parent _GtkWidget]))

(define-gtk widget-window (_fun _GtkWidget -> _GdkWindow)
  #:c-id gtk_widget_get_window
  #:fail (lambda ()
	   (lambda (gtk)
	     (GtkWidgetT-window (cast gtk _GtkWidget _GtkWidgetT-pointer)))))

(define-gtk widget-parent (_fun _GtkWidget -> _GtkWidget)
  #:c-id gtk_widget_get_parent
  #:fail (lambda ()
	   (lambda (gtk)
	     (GtkWidgetT-parent (cast gtk _GtkWidget _GtkWidgetT-pointer)))))

(define-gtk widget-allocation (_fun _GtkWidget (o : (_ptr o _GtkAllocation)) -> _void -> o)
  #:c-id gtk_widget_get_allocation
  #:fail (lambda ()
	   (lambda (gtk)
	     (GtkWidgetT-alloc (cast gtk _GtkWidget _GtkWidgetT-pointer)))))

;; Fallbacks for old Gtk2 versions:
(define ((get-one-flag flag [wrap values]) gtk)
  (wrap (positive? (bitwise-and (get-gtk-object-flags gtk)
				flag))))
(define ((set-one-flag! flag) gtk on?)
  (define v (get-gtk-object-flags gtk))
  (set-gtk-object-flags! gtk 
			 (if on?
			     (bitwise-ior v flag)
			     (bitwise-and v (bitwise-not flag)))))

(define-gtk gtk_widget_has_focus (_fun _GtkWidget -> _gboolean)
  #:fail (lambda () (get-one-flag GTK_HAS_FOCUS)))
(define-gtk gtk_widget_get_mapped (_fun _GtkWidget -> _gboolean)
  #:fail (lambda () (get-one-flag GTK_MAPPED)))
(define-gtk gtk_widget_get_has_window (_fun _GtkWidget -> _gboolean)
  #:fail (lambda () (get-one-flag GTK_NO_WINDOW not)))
(define-gtk gtk_widget_set_can_default (_fun _GtkWidget _gboolean -> _void)
  #:fail (lambda () (set-one-flag! GTK_CAN_DEFAULT)))
(define-gtk gtk_widget_set_can_focus (_fun _GtkWidget _gboolean -> _void)
  #:fail (lambda () (set-one-flag! GTK_CAN_FOCUS)))

(define-gtk gtk_drag_dest_add_uri_targets (_fun _GtkWidget -> _void))
(define-gtk gtk_drag_dest_set (_fun _GtkWidget _int (_pointer = #f) (_int = 0) _int -> _void))
(define-gtk gtk_drag_dest_unset (_fun _GtkWidget -> _void))

(define-gtk gdk_event_get_scroll_deltas (_fun _GdkEventScroll-pointer
					      (dx : (_ptr o _double))
					      (dy : (_ptr o _double))
					      -> _void
					      -> (values dx dy))
  #:make-fail make-not-available)

(define GTK_DEST_DEFAULT_ALL #x07)
(define GDK_ACTION_COPY (arithmetic-shift 1 1))

(define-signal-handler connect-drag-data-received "drag-data-received"
  (_fun _GtkWidget _pointer _int _int _GtkSelectionData _uint _uint -> _void)
  (lambda (gtk context x y data info time)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (let ([bstr (scheme_make_sized_byte_string
                     (gtk_selection_data_get_data data)
                     (gtk_selection_data_get_length data)
                     1)])
          (for ([m (regexp-match* #rx#"file://([^\r]*)\r\n" bstr
                       #:match-select cadr)])
            (queue-window-event wx
                                (lambda ()
                                  (let ([path
                                         (string->path
                                          (uri-decode
                                           (bytes->string/utf-8 m)))])
                                    (send wx on-drop-file path))))))))))

;; ----------------------------------------

(define-signal-handler connect-focus-in "focus-in-event"
  (_fun _GtkWidget _GdkEventFocus-pointer -> _gboolean)
  (lambda (gtk event)
    (let ([wx (gtk->wx gtk)])
      (when wx 
        (send wx focus-change #t)
        (when (send wx on-focus? #t)
          (queue-window-event wx (lambda () (send wx on-set-focus)))))
      #f)))
(define-signal-handler connect-focus-out "focus-out-event"
  (_fun _GtkWidget _GdkEventFocus-pointer -> _gboolean)
  (lambda (gtk event)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx focus-change #f)
        (when (send wx on-focus? #f)
          (queue-window-event wx (lambda () (send wx on-kill-focus)))))
      #f)))
(define (connect-focus gtk)
  (connect-focus-in gtk)
  (connect-focus-out gtk))

(define-signal-handler connect-size-allocate "size-allocate"
  (_fun _GtkWidget _GtkAllocation-pointer -> _gboolean)
  (lambda (gtk a)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx save-size 
              (->normal (GtkAllocation-x a))
              (->normal (GtkAllocation-y a))
              (->normal (GtkAllocation-width a))
              (->normal (GtkAllocation-height a)))))
    #t))
;; ----------------------------------------

(define-signal-handler connect-key-press "key-press-event"
  (_fun _GtkWidget _GdkEventKey-pointer -> _gboolean)
  (lambda (gtk event)
    (do-key-event gtk event #t #f)))

(define-signal-handler connect-key-release "key-release-event"
  (_fun _GtkWidget _GdkEventKey-pointer -> _gboolean)
  (lambda (gtk event)
    (do-key-event gtk event #f #f)))

(define-signal-handler connect-scroll "scroll-event"
  (_fun _GtkWidget _GdkEventScroll-pointer -> _gboolean)
  (lambda (gtk event)
    (do-key-event gtk event #f #t)))

(define scroll-accum-x 0)
(define scroll-accum-y 0)

(define (do-key-event gtk event down? scroll?)
  (let ([wx (gtk->wx gtk)])
    (and
     wx
     (let ([im-str (if scroll?
                       'none
                       ;; Result from `filter-key-event' is one of
                       ;;  - #f => drop the event
                       ;;  - 'none => no replacement; handle as usual
                       ;;  - a string => use as the keycode
                       (send wx filter-key-event event))])
       (when im-str
         (let* ([modifiers (if scroll?
                               (GdkEventScroll-state event)
                               (GdkEventKey-state event))]
                [bit? (lambda (m v) (positive? (bitwise-and m v)))]
                [keyval->code (lambda (kv)
                                (or
                                 (map-key-code kv)
                                 (integer->char (gdk_keyval_to_unicode kv))))]
                [key-code (cond
                           [scroll?
                            (let ([dir (GdkEventScroll-direction event)])
                              (cond
                               [(= dir GDK_SCROLL_UP) 'wheel-up]
                               [(= dir GDK_SCROLL_DOWN) 'wheel-down]
                               [(= dir GDK_SCROLL_LEFT) 'wheel-left]
                               [(= dir GDK_SCROLL_RIGHT) 'wheel-right]
                               [(= dir GDK_SCROLL_SMOOTH)
                                (define-values (dx dy) (gdk_event_get_scroll_deltas event))
                                (set! scroll-accum-x (+ scroll-accum-x dx))
                                (set! scroll-accum-y (+ scroll-accum-y dy))
                                (cond
                                  [(>= scroll-accum-y 1)
                                   (set! scroll-accum-y (sub1 scroll-accum-y))
                                   'wheel-down]
                                  [(<= scroll-accum-y -1)
                                   (set! scroll-accum-y (add1 scroll-accum-y))
                                   'wheel-up]
                                  [(>= scroll-accum-x 1)
                                   (set! scroll-accum-x (sub1 scroll-accum-x))
                                   'wheel-right]
                                  [(<= scroll-accum-x -1)
                                   (set! scroll-accum-x (add1 scroll-accum-x))
                                   'wheel-left]
                                  [else #f])]
                               [else #f]))]
                           [(and (string? im-str)
                                 (= 1 (string-length im-str)))
                            (string-ref im-str 0)]
                           [else
                            (keyval->code (GdkEventKey-keyval event))])]
                [k (new key-event%
                        [key-code key-code]
                        [shift-down (bit? modifiers GDK_SHIFT_MASK)]
                        [control-down (bit? modifiers GDK_CONTROL_MASK)]
                        [meta-down (bit? modifiers GDK_MOD1_MASK)]
                        [mod3-down (bit? modifiers GDK_MOD3_MASK)]
                        [mod4-down (bit? modifiers GDK_MOD4_MASK)]
                        [mod5-down (bit? modifiers GDK_MOD5_MASK)]
                        [alt-down (bit? modifiers GDK_META_MASK)]
                        [x 0]
                        [y 0]
                        [time-stamp (if scroll?
                                        (GdkEventScroll-time event)
                                        (GdkEventKey-time event))]
                        [caps-down (bit? modifiers GDK_LOCK_MASK)])])
           (when (or (and (not scroll?)
                          (let-values ([(s ag sag cl) (get-alts event)]
                                       [(keyval->code*) (lambda (v)
                                                          (and v
                                                               (let ([c (keyval->code v)])
                                                                 (and (not (equal? #\u0000 c))
                                                                      c))))])
                            (let ([s (keyval->code* s)]
                                  [ag (keyval->code* ag)]
                                  [sag (keyval->code* sag)]
                                  [cl (keyval->code* cl)])
                              (when s (send k set-other-shift-key-code s))
                              (when ag (send k set-other-altgr-key-code ag))
                              (when sag (send k set-other-shift-altgr-key-code sag))
                              (when cl (send k set-other-caps-key-code cl))
                              (or s ag sag cl))))
                     (not (equal? #\u0000 key-code)))
             (unless (or scroll? down?)
               ;; swap altenate with main
               (send k set-key-release-code (send k get-key-code))
               (send k set-key-code 'release))
             (if (send wx handles-events? gtk)
                 (begin
                   (queue-window-event wx (lambda () (send wx dispatch-on-char k #f)))
                   #t)
                 (constrained-reply (send wx get-eventspace)
                                    (lambda () (send wx dispatch-on-char k #t))
                                    #t)))))))))

(define-signal-handler connect-button-press "button-press-event"
  (_fun _GtkWidget _GdkEventButton-pointer -> _gboolean)
  (lambda (gtk event)
    (unless (gtk_widget_is_focus gtk)
      (let ([wx (gtk->wx gtk)])
        (when wx
          (unless (other-modal? wx)
            (gtk_widget_grab_focus gtk)))))
    (do-button-event gtk event #f #f)))

(define-signal-handler connect-button-release "button-release-event"
  (_fun _GtkWidget _GdkEventButton-pointer -> _gboolean)
  (lambda (gtk event)
    (do-button-event gtk event #f #f)))

(define-signal-handler connect-pointer-motion "motion-notify-event"
  (_fun _GtkWidget _GdkEventMotion-pointer -> _gboolean)
  (lambda (gtk event)
    (do-button-event gtk event #t #f)))

(define-signal-handler connect-enter "enter-notify-event"
  (_fun _GtkWidget _GdkEventCrossing-pointer -> _gboolean)
  (lambda (gtk event)
    (let ([wx (gtk->wx gtk)]) (when wx (send wx enter-window)))
    (do-button-event gtk event #f #t)))

(define-signal-handler connect-leave "leave-notify-event"
  (_fun _GtkWidget _GdkEventCrossing-pointer -> _gboolean)
  (lambda (gtk event)
    (let ([wx (gtk->wx gtk)]) (when wx (send wx leave-window)))
    (do-button-event gtk event #f #t)))

(define (connect-enter-and-leave gtk)
  (connect-enter gtk)
  (connect-leave gtk))

(define (connect-key gtk)
  (connect-key-press gtk)
  (connect-key-release gtk))

(define (connect-key-and-mouse gtk [skip-press? #f])
  (connect-key gtk)
  (connect-scroll gtk)
  (connect-button-press gtk)
  (unless skip-press? (connect-button-release gtk))
  (connect-pointer-motion gtk)
  (connect-enter-and-leave gtk))

(define (do-button-event gtk event motion? crossing?)
  (let ([type (if motion?
                  GDK_MOTION_NOTIFY
                  (if crossing?
                      (GdkEventCrossing-type event)
                      (GdkEventButton-type event)))])
    (let ([wx (gtk->wx gtk)])
      (when (or (= type GDK_BUTTON_PRESS)
                (= type GDK_2BUTTON_PRESS)
                (= type GDK_3BUTTON_PRESS))
        (let ([floating? (send wx in-floating?)])
          (if floating?
              (gdk_keyboard_grab (widget-window gtk) #t 0)
              (gdk_keyboard_ungrab 0))))
      (and
       wx
       (if (or (= type GDK_2BUTTON_PRESS)
               (= type GDK_3BUTTON_PRESS)
               (and (or (= type GDK_ENTER_NOTIFY)
                        (= type GDK_LEAVE_NOTIFY))
                    (send wx skip-enter-leave-events)))
           #t
           (let* ([modifiers (if motion?
                                 (GdkEventMotion-state event)
                                 (if crossing?
                                     (GdkEventCrossing-state event)
                                     (GdkEventButton-state event)))]
                  [bit? (lambda (m v) (positive? (bitwise-and m v)))]
                  [type (cond
                          [(= type GDK_MOTION_NOTIFY)
                           'motion]
                          [(= type GDK_ENTER_NOTIFY)
                           'enter]
                          [(= type GDK_LEAVE_NOTIFY)
                           'leave]
                          [(= type GDK_BUTTON_PRESS)
                           (case (GdkEventButton-button event)
                             [(1) 'left-down]
                             [(3) 'right-down]
                             [else 'middle-down])]
                          [else
                           (case (GdkEventButton-button event)
                             [(1) 'left-up]
                             [(3) 'right-up]
                             [else 'middle-up])])]
                  [m (let-values ([(x y)
                                   (send wx
                                         adjust-event-position
					 (->normal
					  (->long ((if motion? 
						       GdkEventMotion-x 
						       (if crossing? GdkEventCrossing-x GdkEventButton-x))
						   event)))
					 (->normal
					  (->long ((if motion? GdkEventMotion-y 
						       (if crossing? GdkEventCrossing-y GdkEventButton-y))
						   event))))])
                       (new mouse-event%
                            [event-type type]
                            [left-down (case type
                                         [(left-down) #t]
                                         [(left-up) #f]
                                         [else (bit? modifiers GDK_BUTTON1_MASK)])]
                            [middle-down (case type
                                           [(middle-down) #t]
                                           [(middle-up) #f]
                                           [else (bit? modifiers GDK_BUTTON2_MASK)])]
                            [right-down (case type
                                          [(right-down) #t]
                                          [(right-up) #f]
                                          [else (bit? modifiers GDK_BUTTON3_MASK)])]
                            [x x]
                            [y y]
                            [shift-down (bit? modifiers GDK_SHIFT_MASK)]
                            [control-down (bit? modifiers GDK_CONTROL_MASK)]
                            [meta-down (bit? modifiers GDK_META_MASK)]
                            [alt-down (bit? modifiers GDK_MOD1_MASK)]
                            [mod3-down (bit? modifiers GDK_MOD3_MASK)]
                            [mod4-down (bit? modifiers GDK_MOD4_MASK)]
                            [mod5-down (bit? modifiers GDK_MOD5_MASK)]
                            [time-stamp ((if motion? GdkEventMotion-time 
                                             (if crossing? GdkEventCrossing-time GdkEventButton-time))
                                         event)]
                            [caps-down (bit? modifiers GDK_LOCK_MASK)]))])
             (if (send wx handles-events? gtk)
                 (begin
                   (queue-window-event wx (lambda ()
                                            (send wx dispatch-on-event m #f)))
                   #t)
                 (constrained-reply (send wx get-eventspace)
                                    (lambda () (or (send wx dispatch-on-event m #t)
                                                   (send wx internal-pre-on-event gtk m)))
                                    #t
                                    #:fail-result 
                                    ;; an enter event is synthesized when a button is
                                    ;; enabled and the mouse is over the button, and the
                                    ;; event is not dispatched via the eventspace; leave
                                    ;; events are perhaps similarly synthesized, so allow
                                    ;; them, too
                                    (if (or (eq? type 'enter) (eq? type 'leave))
                                        #f
                                        #t)))))))))

;; ----------------------------------------

(define (internal-error str)
  (log-error
   (apply string-append
          (format "internal error: ~a" str)
          (append
           (for/list ([c (continuation-mark-set->context (current-continuation-marks))])
             (let ([name (car c)]
                   [loc (cdr c)])
               (cond
                [loc
                 (string-append
                  "\n"
                  (cond 
                   [(srcloc-line loc)
                    (format "~a:~a:~a" 
                            (srcloc-source loc)
                            (srcloc-line loc)
                            (srcloc-column loc))]
                   [else
                    (format "~a::~a" 
                            (srcloc-source loc)
                            (srcloc-position loc))])
                  (if name (format " ~a" name) ""))]
                [else (format "\n ~a" name)])))
           '("\n")))))

(define window%
  (class widget%
    (init-field parent
                gtk)
    (init [no-show? #f]
          [extra-gtks null]
          [add-to-parent? #t]
          [connect-size-allocate? #t])

    (super-new [gtk gtk]
               [extra-gtks extra-gtks]
               [parent parent])

    (define save-x (get-unset-pos))
    (define save-y (get-unset-pos))
    (define save-w 0)
    (define save-h 0)

    (define/public (get-unset-pos) 0)

    (when connect-size-allocate?
      (connect-size-allocate gtk))

    (when add-to-parent?
      (gtk_container_add (send parent get-container-gtk) gtk))

    (define/public (get-gtk) gtk)
    (define/public (get-client-gtk) gtk)
    (define/public (get-container-gtk) (get-client-gtk))
    (define/public (get-window-gtk) (send parent get-window-gtk))

    (define/public (move x y)
      (set-size x y -1 -1))

    (define/public (set-size x y w h)
      (unless (and (or (not x) (equal? save-x x))
                   (or (not y) (equal? save-y y))
                   (or (= w -1) (= save-w (max w client-delta-w)))
                   (or (= h -1) (= save-h (max h client-delta-h))))
        (unless (not x) (set! save-x x))
        (unless (not y) (set! save-y y))
        (unless (= w -1) (set! save-w w))
        (unless (= h -1) (set! save-h h))
        (set! save-w (max save-w client-delta-w))
        (set! save-h (max save-h client-delta-h))
        (really-set-size gtk x y (or save-x 0) (or save-y 0) save-w save-h)
        (queue-on-size)))

    (define/public (save-size x y w h)
      (set! save-w w)
      (set! save-h h))

    (define/public (really-set-size gtk given-x given-y x y w h)
      (send parent set-child-size gtk x y w h))

    (define/public (set-child-size child-gtk x y w h)
      (gtk_widget_set_size_request child-gtk (->screen w) (->screen h))
      (gtk_widget_size_allocate child-gtk (make-GtkAllocation (->screen x) (->screen y)
							      (->screen w) (->screen h))))

    (define/public (remember-size x y w h)
      ;; called in event-pump thread
      (unless (and (= save-w w)
                   (= save-h h)
                   (equal? save-x x)
                   (equal? save-y y))
        (set! save-w w)
        (set! save-h h)
        (set! save-x x)
        (set! save-y y)
        (queue-on-size)))

    (define/public (queue-on-size) (void))

    (define client-delta-w 0)
    (define client-delta-h 0)

    (define/public (adjust-client-delta dw dh)
      (set! client-delta-w dw)
      (set! client-delta-h dh))

    (define/public (infer-client-delta [w? #t] [h? #t] [sub-h-gtk #f]
				       #:inside [inside-gtk (get-container-gtk)])
      (let ([req (make-GtkRequisition 0 0)]
            [creq (make-GtkRequisition 0 0)]
            [hreq (make-GtkRequisition 0 0)])
	(when gtk3? (gtk_widget_show gtk))
        (gtk_widget_size_request gtk req)
        (gtk_widget_size_request inside-gtk creq)
        (when sub-h-gtk
          (gtk_widget_size_request sub-h-gtk hreq))
        (when w?
          (set! client-delta-w (->normal
				(- (GtkRequisition-width req)
				   (max (GtkRequisition-width creq)
					(GtkRequisition-width hreq))))))
        (when h?
          (set! client-delta-h (->normal
				(- (GtkRequisition-height req)
				   (GtkRequisition-height creq)))))
	(when gtk3? (gtk_widget_hide gtk))))

    (define/public (set-auto-size [dw 0] [dh 0])
      (let ([req (make-GtkRequisition 0 0)])
	(cond
	 [gtk3?
	  (unless shown? (gtk_widget_show gtk))
	  (gtk_widget_get_preferred_size gtk req #f)
	  (unless shown? (gtk_widget_hide gtk))]
	 [else (gtk_widget_size_request gtk req)])
        (set-size #f
                  #f
                  (+ (->normal (GtkRequisition-width req)) dw)
                  (+ (->normal (GtkRequisition-height req)) dh))))

    (define shown? #f)
    (define/public (direct-show on?)
      ;; atomic mode
      (if on?
	  (gtk_widget_show gtk)
          (gtk_widget_hide gtk))
      (set! shown? (and on? #t))
      (register-child-in-parent on?)
      (when on? (reset-child-dcs)))
    (define/public (show on?)
      (atomically
       (direct-show on?)))
    (define/public (reset-child-freezes) (void))
    (define/public (reset-child-dcs) (void))
    (define/public (is-shown?) shown?)
    (define/public (is-shown-to-root?)
      (and shown?
           (if parent
               (send parent is-shown-to-root?)
               #t)))

    (unless no-show? (show #t))

    (define/public (get-x) (or save-x 0))
    (define/public (get-y) (or save-y 0))
    (define/public (get-width) save-w)
    (define/public (get-height) save-h)

    (define/public (get-parent) parent)
    (define/public (set-parent p)
      ;; in atomic mode
      (reset-child-freezes)
      (g_object_ref gtk)
      (gtk_container_remove (send parent get-container-gtk) gtk)
      (set! parent p)
      (gtk_container_add (send parent get-container-gtk) gtk)
      (set! save-x 0)
      (set! save-y 0)
      (g_object_unref gtk))

    (define/public (get-top-win) (send parent get-top-win))

    (define/public (get-dialog-level) (send parent get-dialog-level))

    (define/public (get-size xb yb)
      (set-box! xb save-w)
      (set-box! yb save-h))
    (define/public (get-client-size xb yb)
      (get-size xb yb)
      (set-box! xb (max 0 (- (unbox xb) client-delta-w)))
      (set-box! yb (max 0 (- (unbox yb) client-delta-h))))

    (define enabled? #t)
    (define/pubment (is-enabled-to-root?)
      (and enabled?
           (inner (send parent is-enabled-to-root?)
                  is-enabled-to-root?)))
    (define/public (enable on?)
      (set! enabled? on?)
      (gtk_widget_set_sensitive gtk on?))
    (define/public (is-window-enabled?) enabled?)

    (define drag-connected? #f)
    (define/public (drag-accept-files on?)
      (if on?
          (begin
            (unless drag-connected?
              (connect-drag-data-received gtk)
              (set! drag-connected? #t))
            (gtk_drag_dest_set gtk GTK_DEST_DEFAULT_ALL GDK_ACTION_COPY)
            (gtk_drag_dest_add_uri_targets gtk))
          (gtk_drag_dest_unset gtk)))
      
    (define/public (in-floating?)
      (send parent in-floating?))

    (define/public (set-focus)
      (define gtk (get-client-gtk))
      (gtk_widget_grab_focus gtk)
      ;; Force focus to or away from a floating window:
      (cond
       [(and (in-floating?)
	     (is-shown-to-root?))
	(gdk_keyboard_grab (widget-window gtk) #t 0)]
       [else
	(gdk_keyboard_ungrab 0)]))

    (define cursor-handle #f)
    (define/public (set-cursor v)
      (set! cursor-handle (and v
                               (send (send v get-driver) get-handle)))
      (check-window-cursor this))
    (define/public (enter-window)
      (set-window-cursor this #f))
    (define/public (leave-window)
      (when parent
        (send parent enter-window)))
    (define/public (set-window-cursor in-win c)
      (set-parent-window-cursor in-win (or c cursor-handle)))
    (define/public (set-parent-window-cursor in-win c)
      (when parent
        (send parent set-window-cursor in-win c)))
    (define/public (check-window-cursor win)
      (when parent
        (send parent check-window-cursor win)))

    (define/public (on-set-focus) (void))
    (define/public (on-kill-focus) (void))

    (define/public (focus-change on?) (void))
    (define/public (filter-key-event e) 'none)

    (define/public (on-focus? on?) #t)

    (define/private (pre-event-refresh)
      ;; Since we break the connection between the
      ;; Gtk queue and event handling, we
      ;; re-sync the display in case a stream of
      ;; events (e.g., key repeat) have a corresponding
      ;; stream of screen updates.
      (flush-display))

    (define/public (handles-events? gtk) #f)
    (define/public (dispatch-on-char e just-pre?) 
      (pre-event-refresh)
      (cond
       [(other-modal? this) #t]
       [(call-pre-on-char this e) #t]
       [just-pre? #f]
       [else (when enabled? (on-char e)) #t]))
    (define/public (dispatch-on-event e just-pre?) 
      (pre-event-refresh)
      (cond
       [(other-modal? this e) #t]
       [(call-pre-on-event this e) #t]
       [just-pre? #f]
       [else (when enabled? (on-event e)) #t]))

    (define/public (internal-pre-on-event gtk e) #f)

    (define/public (call-pre-on-event w e)
      (or (send parent call-pre-on-event w e)
          (pre-on-event w e)))
    (define/public (call-pre-on-char w e)
      (or (send parent call-pre-on-char w e)
          (pre-on-char w e)))
    (define/public (pre-on-event w e) #f)
    (define/public (pre-on-char w e) #f)

    (define/public (on-char e) (void))
    (define/public (on-event e) (void))

    (define skip-enter-leave? #f)
    (define/public skip-enter-leave-events 
      (case-lambda
       [(skip?) (set! skip-enter-leave? skip?)]
       [else skip-enter-leave?]))

    (define/public (register-child child on?)
      (void))
    (define/public (register-child-in-parent on?)
      (when parent
        (send parent register-child this on?)))

    (define/public (paint-children)
      (void))

    (define/public (on-drop-file path) (void))

    (define/public (get-handle) (get-gtk))
    (define/public (get-client-handle) (get-container-gtk))

    (define/public (popup-menu m x y)
      (let ([gx (box x)]
            [gy (box y)])
        (client-to-screen gx gy)
        (send m popup (unbox gx) (unbox gy)
              (lambda (thunk) (queue-window-event this thunk)))))

    (define/public (center a b) (void))
    (define/public (refresh) (refresh-all-children))

    (define/public (refresh-all-children) (void))

    (define/public (screen-to-client x y)
      (internal-screen-to-client x y))
    (define/public (internal-screen-to-client x y)
      (let ([xb (box 0)]
            [yb (box 0)])
        (internal-client-to-screen xb yb)
        (set-box! x (- (unbox x) (unbox xb)))
        (set-box! y (- (unbox y) (unbox yb)))))
    (define/public (client-to-screen x y)
      (internal-client-to-screen x y))
    (define/public (internal-client-to-screen x y)
      (let-values ([(dx dy) (get-client-delta)])
        (send parent internal-client-to-screen x y)
        (set-box! x (+ (unbox x) save-x dx))
        (set-box! y (+ (unbox y) save-y dy))))

    (define event-position-wrt-wx #f)
    (define/public (set-event-positions-wrt wx)
      (set! event-position-wrt-wx wx))
    
    (define/public (adjust-event-position x y)
      (if event-position-wrt-wx
          (let ([xb (box x)]
                [yb (box y)])
            (internal-client-to-screen xb yb)
            (send event-position-wrt-wx internal-screen-to-client xb yb)
            (values (unbox xb) (unbox yb)))
          (values x y)))

    (define/public (get-client-delta)
      (values 0 0))
    (define/public (get-stored-client-delta)
      (values client-delta-w client-delta-h))

    (define/public (warp-pointer x y)
      (define xb (box x))
      (define yb (box y))
      (client-to-screen xb yb)
      (gdk_display_warp_pointer (gtk_widget_get_display gtk)
                                (gtk_widget_get_screen gtk)
                                (->screen (unbox xb))
                                (->screen (unbox yb))))

    (define/public (gets-focus?) #t)))

(define (queue-window-event win thunk)
  (queue-event (send win get-eventspace) thunk))
(define (queue-window-refresh-event win thunk)
  (queue-refresh-event (send win get-eventspace) thunk))

(define-gdk gdk_display_flush (_fun _GdkDisplay -> _void))
(define-gdk gdk_display_get_default (_fun -> _GdkDisplay))
(define (flush-display)
  (try-to-sync-refresh)
  (gdk_window_process_all_updates)
  (gdk_display_flush (gdk_display_get_default)))

(define-gdk gdk_window_freeze_updates (_fun _GdkWindow -> _void))
(define-gdk gdk_window_thaw_updates (_fun _GdkWindow -> _void))
(define-gdk gdk_window_invalidate_rect (_fun _GdkWindow _pointer _gboolean -> _void))
(define-gdk gdk_window_process_all_updates (_fun -> _void))
(define-gdk gdk_window_ensure_native (_fun _GdkWindow -> _gboolean)
  ;; Requires 2.18
  #:fail (lambda () (lambda (win) #f)))

(define (win-box-valid? win-box)
  (mcar win-box))
(define (window->win-box win)
  (mcons win 0))
(define (unrealize-win-box win-box)
  (let ([win (mcar win-box)])
    (when win
      (set-mcar! win-box #f)
      (for ([i (in-range (mcdr win-box))])
        (gdk_window_thaw_updates win)))))

(define (request-flush-delay win-box transparentish?)
  (do-request-flush-delay 
   win-box
   (lambda (win-box)
     (let ([win (mcar win-box)])
       (and win
            ;; The freeze/thaw state is actually with the window's
            ;; implementation, so force a native implementation of the
            ;; window to try to avoid it changing out from underneath
            ;; us between the freeze and thaw actions.
	    ;; With Gtk3, we can't use a native window for transparent
	    ;; windows; that means we have to be extra careful that
	    ;; the underlying window doesn't change while a freeze is
	    ;; in effect; the `reset-child-freezes` helps with that.
            (unless (and transparentish? gtk3?)
              (gdk_window_ensure_native win))
            (begin
              (gdk_window_freeze_updates win)
              (set-mcdr! win-box (add1 (mcdr win-box)))
              #t))))
   (lambda (win-box)
     (let ([win (mcar win-box)])
       (when win
         (gdk_window_thaw_updates win)
         (set-mcdr! win-box (sub1 (mcdr win-box))))))))

(define (cancel-flush-delay req)
  (when req
    (do-cancel-flush-delay 
     req
     (lambda (win-box)
       (let ([win (mcar win-box)])
         (when win
           (gdk_window_thaw_updates win)
           (set-mcdr! win-box (sub1 (mcdr win-box)))))))))
